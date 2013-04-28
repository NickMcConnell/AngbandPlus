/* File: init2.c */


/*
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
	ANGBAND_DIR_USER = string_make("");
	ANGBAND_DIR_XTRA = string_make("");


#else /* VM */


	/*** Build the sub-directory names ***/

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
	strcpy(tail, "save");
	ANGBAND_DIR_SAVE = string_make(path);

	/* Build a path name */
	strcpy(tail, "user");
	ANGBAND_DIR_USER = string_make(path);

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



#ifdef ALLOW_TEMPLATES


/*
 * Hack -- help give useful error messages
 */
s16b error_idx;
s16b error_line;


/*
 * Hack -- help initialize the fake "name" and "text" arrays when
 * parsing an "ascii" template file.
 */
u16b fake_name_size;
u16b fake_text_size;


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
	f_head->v_major = VERSION_MAJOR;
	f_head->v_minor = VERSION_MINOR;
	f_head->v_patch = VERSION_PATCH;
	f_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	f_head->info_num = MAX_F_IDX;
	f_head->info_len = sizeof(feature_type);

	/* Save the size of "f_head" and "f_info" */
	f_head->head_size = sizeof(header);
	f_head->info_size = f_head->info_num * f_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "f_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_f_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'f_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Fake the size of "f_name" and "f_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "f_info" array */
	C_MAKE(f_info, f_head->info_num, feature_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(f_name, fake_name_size, char);
	C_MAKE(f_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "f_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'f_info.txt' file.");

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
		msg_format("Error %d at line %d of 'f_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'f_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "f_info.raw");

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
	C_KILL(f_name, fake_name_size, char);
	C_KILL(f_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "f_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'f_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_f_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'f_info.raw' file.");

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
	k_head->v_major = VERSION_MAJOR;
	k_head->v_minor = VERSION_MINOR;
	k_head->v_patch = VERSION_PATCH;
	k_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	k_head->info_num = MAX_K_IDX;
	k_head->info_len = sizeof(object_kind);

	/* Save the size of "k_head" and "k_info" */
	k_head->head_size = sizeof(header);
	k_head->info_size = k_head->info_num * k_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "k_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_k_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'k_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Fake the size of "k_name" and "k_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "k_info" array */
	C_MAKE(k_info, k_head->info_num, object_kind);

	/* Hack -- make "fake" arrays */
	C_MAKE(k_name, fake_name_size, char);
	C_MAKE(k_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "k_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'k_info.txt' file.");

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
		msg_format("Error %d at line %d of 'k_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'k_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "k_info.raw");

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
	C_KILL(k_name, fake_name_size, char);
	C_KILL(k_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "k_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'k_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_k_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'k_info.raw' file.");

	/* Success */
	return (0);
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
	a_head->v_major = VERSION_MAJOR;
	a_head->v_minor = VERSION_MINOR;
	a_head->v_patch = VERSION_PATCH;
	a_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	a_head->info_num = MAX_A_IDX;
	a_head->info_len = sizeof(artifact_type);

	/* Save the size of "a_head" and "a_info" */
	a_head->head_size = sizeof(header);
	a_head->info_size = a_head->info_num * a_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "a_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_a_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'a_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Fake the size of "a_name" and "a_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "a_info" array */
	C_MAKE(a_info, a_head->info_num, artifact_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(a_name, fake_name_size, char);
	C_MAKE(a_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "a_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'a_info.txt' file.");

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
		msg_format("Error %d at line %d of 'a_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'a_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "a_info.raw");

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
	C_KILL(a_name, fake_name_size, char);
	C_KILL(a_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "a_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot open 'a_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_a_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'a_info.raw' file.");

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
	e_head->v_major = VERSION_MAJOR;
	e_head->v_minor = VERSION_MINOR;
	e_head->v_patch = VERSION_PATCH;
	e_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	e_head->info_num = MAX_E_IDX;
	e_head->info_len = sizeof(ego_item_type);

	/* Save the size of "e_head" and "e_info" */
	e_head->head_size = sizeof(header);
	e_head->info_size = e_head->info_num * e_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "e_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_e_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'e_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Fake the size of "e_name" and "e_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "e_info" array */
	C_MAKE(e_info, e_head->info_num, ego_item_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(e_name, fake_name_size, char);
	C_MAKE(e_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "e_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'e_info.txt' file.");

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
		msg_format("Error %d at line %d of 'e_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'e_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "e_info.raw");

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
	C_KILL(e_name, fake_name_size, char);
	C_KILL(e_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "e_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'e_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_e_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'e_info.raw' file.");

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
	r_head->v_major = VERSION_MAJOR;
	r_head->v_minor = VERSION_MINOR;
	r_head->v_patch = VERSION_PATCH;
	r_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	r_head->info_num = MAX_R_IDX;
	r_head->info_len = sizeof(monster_race);

	/* Save the size of "r_head" and "r_info" */
	r_head->head_size = sizeof(header);
	r_head->info_size = r_head->info_num * r_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "r_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_r_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'r_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Assume the size of "r_name" and "r_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "r_info" array */
	C_MAKE(r_info, r_head->info_num, monster_race);

	/* Hack -- make "fake" arrays */
	C_MAKE(r_name, fake_name_size, char);
	C_MAKE(r_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "r_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'r_info.txt' file.");

	/* Parse the file */
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
		msg_format("Error %d at line %d of 'r_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'r_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "r_info.raw");

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
	C_KILL(r_name, fake_name_size, char);
	C_KILL(r_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "r_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'r_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_r_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'r_info.raw' file.");

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
	v_head->v_major = VERSION_MAJOR;
	v_head->v_minor = VERSION_MINOR;
	v_head->v_patch = VERSION_PATCH;
	v_head->v_extra = VERSION_EXTRA;

	/* Save the "record" information */
	v_head->info_num = MAX_V_IDX;
	v_head->info_len = sizeof(vault_type);

	/* Save the size of "v_head" and "v_info" */
	v_head->head_size = sizeof(header);
	v_head->info_size = v_head->info_num * v_head->info_len;


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "v_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd >= 0)
	{
		/* Attempt to parse the "raw" file */
		err = init_v_info_raw(fd);

		/* Close it */
		fd_close(fd);

		/* Success */
		if (!err) return (0);

		/* Information */
		msg_print("Ignoring obsolete/defective 'v_info.raw' file.");
		msg_print(NULL);
	}


	/*** Make the fake arrays ***/

	/* Fake the size of "v_name" and "v_text" */
	fake_name_size = 20 * 1024L;
	fake_text_size = 60 * 1024L;

	/* Allocate the "k_info" array */
	C_MAKE(v_info, v_head->info_num, vault_type);

	/* Hack -- make "fake" arrays */
	C_MAKE(v_name, fake_name_size, char);
	C_MAKE(v_text, fake_text_size, char);


	/*** Load the ascii template file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "v_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit("Cannot open 'v_info.txt' file.");

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
		msg_format("Error %d at line %d of 'v_info.txt'.", err, error_line);
		msg_format("Record %d contains a '%s' error.", error_idx, oops);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);

		/* Quit */
		quit("Error in 'v_info.txt' file.");
	}


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "v_info.raw");

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
	C_KILL(v_name, fake_name_size, char);
	C_KILL(v_text, fake_text_size, char);

	/* Forget the array sizes */
	fake_name_size = 0;
	fake_text_size = 0;

#endif	/* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, "v_info.raw");

	/* Attempt to open the "raw" file */
	fd = fd_open(buf, O_RDONLY);

	/* Process existing "raw" file */
	if (fd < 0) quit("Cannot load 'v_info.raw' file.");

	/* Attempt to parse the "raw" file */
	err = init_v_info_raw(fd);

	/* Close it */
	fd_close(fd);

	/* Error */
	if (err) quit("Cannot parse 'v_info.raw' file.");

	/* Success */
	return (0);
}




/*** Initialize others ***/



/*
 * Hack -- Objects sold in the stores -- by tval/sval pair.
 */
static byte store_table[MAX_STORES-2][STORE_CHOICES][2] =
{
	{
		/* General Store */

		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
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
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_AUGMENTED_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_BAR_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_DOUBLE_CHAIN_MAIL },

		{ TV_HARD_ARMOR, SV_METAL_BRIGANDINE_ARMOUR },
		{ TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
		{ TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
		{ TV_GLOVES, SV_SET_OF_GAUNTLETS },
		{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_METAL_SHIELD }
	},

	{
		/* Weaponsmith */

		{ TV_SWORD, SV_DAGGER },
		{ TV_SWORD, SV_MAIN_GAUCHE },
		{ TV_SWORD, SV_RAPIER },
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_SWORD, SV_SHORT_SWORD },
		{ TV_SWORD, SV_SABRE },
		{ TV_SWORD, SV_CUTLASS },
		{ TV_SWORD, SV_TULWAR },

		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SWORD, SV_LONG_SWORD },
		{ TV_SWORD, SV_SCIMITAR },
		{ TV_SWORD, SV_KATANA },
		{ TV_SWORD, SV_BASTARD_SWORD },
		{ TV_POLEARM, SV_SPEAR },
		{ TV_POLEARM, SV_AWL_PIKE },
		{ TV_POLEARM, SV_TRIDENT },

		{ TV_POLEARM, SV_PIKE },
		{ TV_POLEARM, SV_BEAKED_AXE },
		{ TV_POLEARM, SV_BROAD_AXE },
		{ TV_POLEARM, SV_LANCE },
		{ TV_POLEARM, SV_BATTLE_AXE },
		{ TV_HAFTED, SV_WHIP },
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
		/* Temple */

		{ TV_HAFTED, SV_WHIP },
		{ TV_HAFTED, SV_QUARTERSTAFF },
		{ TV_HAFTED, SV_MACE },
		{ TV_HAFTED, SV_MACE },
		{ TV_HAFTED, SV_BALL_AND_CHAIN },
		{ TV_HAFTED, SV_WAR_HAMMER },
		{ TV_HAFTED, SV_LUCERN_HAMMER },
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
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },

		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 0 },
		{ TV_PRAYER_BOOK, 1 },
		{ TV_PRAYER_BOOK, 1 },
		{ TV_PRAYER_BOOK, 2 },
		{ TV_PRAYER_BOOK, 2 },
		{ TV_PRAYER_BOOK, 3 }
	},

	{
		/* Alchemy shop */

		{ TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_HIT },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_DAM },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_ARMOR },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_LIGHT },

		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_PHASE_DOOR },
		{ TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION },
		{ TV_SCROLL, SV_SCROLL_MAPPING },
		{ TV_SCROLL, SV_SCROLL_DETECT_GOLD },
		{ TV_SCROLL, SV_SCROLL_DETECT_ITEM },
		{ TV_SCROLL, SV_SCROLL_DETECT_TRAP },

		{ TV_SCROLL, SV_SCROLL_DETECT_DOOR },
		{ TV_SCROLL, SV_SCROLL_DETECT_INVIS },
		{ TV_SCROLL, SV_SCROLL_RECHARGING },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },

		{ TV_POTION, SV_POTION_RESIST_HEAT },
		{ TV_POTION, SV_POTION_RESIST_COLD },
		{ TV_POTION, SV_POTION_RES_STR },
		{ TV_POTION, SV_POTION_RES_INT },
		{ TV_POTION, SV_POTION_RES_WIS },
		{ TV_POTION, SV_POTION_RES_DEX },
		{ TV_POTION, SV_POTION_RES_CON },
		{ TV_POTION, SV_POTION_RES_CHR }
	},

	{
		/* Magic-User store */

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
		{ TV_WAND, SV_WAND_WONDER },
		{ TV_STAFF, SV_STAFF_LITE },
		{ TV_STAFF, SV_STAFF_MAPPING },
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

		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 0 },
		{ TV_MAGIC_BOOK, 1 },
		{ TV_MAGIC_BOOK, 1 },
		{ TV_MAGIC_BOOK, 2 },
		{ TV_MAGIC_BOOK, 2 },
		{ TV_MAGIC_BOOK, 3 }
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
		if ((i == STORE_B_MARKET) || (i == STORE_HOME)) continue;

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


	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s (%s).", "Ben Harrison", MAINTAINER);


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

	alloc_entry *table;

	s16b num[MAX_DEPTH];

	s16b aux[MAX_DEPTH];


	/*** Analyze object allocation info ***/

	/* Clear the "aux" array */
	C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	C_WIPE(&num, MAX_DEPTH, s16b);

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
	C_WIPE(&aux, MAX_DEPTH, s16b);

	/* Clear the "num" array */
	C_WIPE(&num, MAX_DEPTH, s16b);

	/* Size of "alloc_race_table" */
	alloc_race_size = 0;

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < MAX_R_IDX - 1; i++)
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

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < MAX_R_IDX - 1; i++)
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

	/* Initialize ego-item info */
	note("[Initializing arrays... (ego-items)]");
	if (init_e_info()) quit("Cannot initialize ego-items");

	/* Initialize monster info */
	note("[Initializing arrays... (monsters)]");
	if (init_r_info()) quit("Cannot initialize monsters");

	/* Initialize feature info */
	note("[Initializing arrays... (vaults)]");
	if (init_v_info()) quit("Cannot initialize vaults");

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



/*
 * Random artifact generator (randart) by Greg Wooledge.
 *
 * Taken directly from "randart.c" with no significant modifications,
 * though obviously at some point some stylistic modifications will be
 * desirable, in particular, involving the order of functions, the use
 * of spaces before function parens, the use of "do/while", and the lack
 * of "static" on various functions....  :-)
 */

#ifdef GJW_RANDART

#define MAX_TRIES 200
#define BUFLEN 1024

#define NAMES_FILE "names.txt"
#define MIN_NAME_LEN 5
#define MAX_NAME_LEN 9
#define S_WORD 26
#define E_WORD S_WORD
long lprobs[S_WORD+1][S_WORD+1][S_WORD+1];	/* global, hence init to 0 */
long ltotal[S_WORD+1][S_WORD+1];		/* global, hence init to 0 */

/* Temporary space for names, while reading and randomizing them. */
char *names[MAX_A_IDX];
int nnames = 0;

/* Cache the results of lookup_kind(), which is expensive and would otherwise
   be called much too often. */
s16b kinds[MAX_A_IDX];

/* Global just for convenience. */
int randart_verbose = 0;

int init_names (void);
void build_prob (FILE *learn);
char *make_word (void);
int scramble (void);
int scramble_artifact (int a_idx);
void choose_item (int a_idx, u32b activates);
void add_ability (artifact_type *);
void remove_contradictory (artifact_type *);
void do_pval (artifact_type *);
void do_curse (artifact_type *);
s32b artifact_power (int a_idx, bool cannot_use_kind_cache);
int artifacts_acceptable (void);
int bow_multiplier (int sval);
char *my_strdup (const char *);

#define abs(x)	((x) > 0 ? (x) : (-(x)))
#define sign(x)	((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))

int do_randart (u32b randart_seed)
{
	int rc;

	/* Prepare to use the Angband "simple" RNG.  We want this to follow
	   the precedents set by the town layout and object flavor code. */
	Rand_value = randart_seed;
	Rand_quick = TRUE;

	msg_format ("Reading new artifact names...");
	if ((rc = init_names()) != 0) return rc;

	msg_format ("Scrambling artifacts...");
	if ((rc = scramble()) != 0) return rc;

	/* When done, resume use of the Angband "complex" RNG.  Right
	   now this does nothing, but later it will be important! */
	Rand_quick = FALSE;

	return 0;
}

/* Use W. Sheldon Simms' random name generator. */
int init_names (void)
{
	char buf [BUFLEN];
	FILE *f;
	size_t name_size;
	char *a_base;
	char *a_next;
	int i;

	path_build (buf, BUFLEN, ANGBAND_DIR_FILE, NAMES_FILE);
	if ((f = fopen (buf, "r")) == NULL)
	{
		msg_format ("could not open '" NAMES_FILE "'\n");
		return 1;
	}
	build_prob (f);
	fclose (f);

	for (i = 0; i < MAX_A_IDX; i++)
	{
		char *word = make_word();

		if (rand_int (3) == 0)
			sprintf (buf, "'%s'", word);
		else
			sprintf (buf, "of %s", word);
		names[i] = my_strdup (buf);
	}

	/* Special cases -- keep these three names separate. */
	free (names [ART_POWER - 1]);
	free (names [ART_GROND - 1]);
	free (names [ART_MORGOTH - 1]);
	if ((names [ART_POWER - 1] = my_strdup ("of Power (The One Ring)")) == NULL)
	{
		msg_format ("Memory allocation error");
		return 1;
	}
	if ((names [ART_GROND - 1] = my_strdup ("'Grond'")) == NULL)
	{
		msg_format ("Memory allocation error");
		return 1;
	}
	if ((names [ART_MORGOTH - 1] = my_strdup ("of Morgoth")) == NULL)
	{
		msg_format ("Memory allocation error");
		return 1;
	}

	/* Convert our names array into an a_name structure for later use. */
	name_size = 0;
	for (i = 1; i < MAX_A_IDX; i++)
	{
		name_size += strlen (names[i-1]) + 2;	/* skip first char */
	}
	if ((a_base = malloc (name_size)) == NULL)
	{
		msg_format ("Memory allocation error");
		return 1;
	}
	a_next = a_base + 1;	/* skip first char */
	for (i = 1; i < MAX_A_IDX; i++)
	{
		strcpy (a_next, names[i-1]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen (names[i-1]) + 1;
	}

	/* Free some of our now unneeded memory. */
	KILL (a_name, char);
	for (i = 0; i < nnames; i++)
	{
		free (names [i]);
	}
	a_name = a_base;

	return 0;
}

/* Use W. Sheldon Simms' random name generator.  This function builds
   probability tables which are used later on for letter selection.  It
   relies on the ASCII character set. */
void build_prob (FILE *learn)
{
	int c_prev, c_cur, c_next;

	/* Build raw frequencies */
	while (1)
	{
		c_prev = c_cur = S_WORD;

		do
		{
			c_next = fgetc (learn);
		} while (!isalpha (c_next) && (c_next != EOF));
		if (c_next == EOF) break;

		do
		{
			c_next = tolower (c_next) - 'a';	/* ASCII */
			lprobs[c_prev][c_cur][c_next]++;
			ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
			c_next = fgetc (learn);
		} while (isalpha (c_next));

		lprobs [c_prev][c_cur][E_WORD]++;
		ltotal [c_prev][c_cur]++;
	}
}

/* Use W. Sheldon Simms' random name generator.  Generate a random word using
   the probability tables we built earlier.  Relies on the ASCII character
   set.  Relies on European vowels (a, e, i, o, u).  The generated name should
   be copied/used before calling this function again. */
char *make_word (void)
{
	static char word_buf [90];
	int r, totalfreq;
	int tries, lnum, vow;
	int c_prev, c_cur, c_next;
	char *cp;

    startover:
	vow = 0;
	lnum = 0;
	tries = 0;
	cp = word_buf;
	c_prev = c_cur = S_WORD;

	while (1)
	{
	    getletter:
		c_next = 0;
		r = rand_int (ltotal [c_prev][c_cur]);
		totalfreq = lprobs [c_prev][c_cur][c_next];
		while (totalfreq <= r)
		{
			c_next++;
			totalfreq += lprobs [c_prev][c_cur][c_next];
		}

		if (c_next == E_WORD)
		{
			if ((lnum < MIN_NAME_LEN) || vow == 0)
			{
				tries++;
				if (tries < 10) goto getletter;
				goto startover;
			}
			*cp = '\0';
			break;
		}
		if (lnum >= MAX_NAME_LEN) goto startover;

		*cp = c_next + 'a';	/* ASCII */
		switch (*cp)
		{
			case 'a': case 'e': case 'i': case 'o': case 'u':
				vow++;
		}

		cp++;
		lnum++;
		c_prev = c_cur;
		c_cur = c_next;
	}

	word_buf[0] = toupper (word_buf[0]);
	return word_buf;
}

int scramble (void)
{
	/* This outer loop is for post-processing.  If our artifact set
	   fails to meet certain criteria, we start over. :-( */
	do
	{
		int a_idx;

		/* Generate all the artifacts. */
		for (a_idx = 1; a_idx < MAX_A_IDX; a_idx++)
		{
			scramble_artifact (a_idx);
		}
	} while (!artifacts_acceptable());	/* end of all artifacts */

	return 0;
}

/* Note the three special cases (One Ring, Grond, Morgoth).  Note also that if
   an artifact has an activation, it must be preserved by artifact number,
   since activations are hard-coded into the game. */
int scramble_artifact (int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	u32b activates = a_ptr->flags3 & TR3_ACTIVATE;
	s32b power;
	int tries;
	s32b ap;
	bool curse_me = FALSE;
	bool aggravate_me = FALSE;

	/* Special cases -- don't randomize these! */
	if (a_idx == ART_POWER || a_idx == ART_GROND ||
	    a_idx == ART_MORGOTH)
		return 0;

	/* Skip unused artifacts, too! */
	if (a_ptr->tval == 0) return 0;

	/* Evaluate the original artifact to determine the power
	   level. */
	power = artifact_power (a_idx, TRUE);
	if (power < 0) curse_me = TRUE;

	if (randart_verbose)
		msg_format ("Artifact %d: power = %d", a_idx, power);

	/* Really powerful items should aggravate. */
	if (power > 100)
	{
		if (rand_int (100) < (power - 100) * 3)
		{
			aggravate_me = TRUE;
		}
	}

	if (a_idx >= ART_MIN_NORMAL)
	{
		/* Normal artifact - choose a random base item type.  Not too
		   powerful, so we'll have to add something to it.  Not too
		   weak, for the opposite reason. */
		int count = 0;
		s32b ap2;
		do
		{
			choose_item (a_idx, activates);
			ap2 = artifact_power (a_idx, FALSE);
			count++;
		} while ((count < MAX_TRIES) &&
			   ((ap2 > (power * 8) / 10 + 1) ||
			    (ap2 < (power / 10))));
	}
	else
	{
		/* Special artifact (light source, ring, or
		   amulet).  Clear the following fields; leave
		   the rest alone. */
		a_ptr->pval = 0;
		a_ptr->to_h = a_ptr->to_d = a_ptr->to_a = 0;
		a_ptr->flags1 = a_ptr->flags2 = 0;
		a_ptr->flags3 = (TR3_IGNORE_ACID |
				 TR3_IGNORE_ELEC |
				 TR3_IGNORE_FIRE |
				 TR3_IGNORE_COLD);
	}

	/* First draft: add two abilities, then curse it three times. */
	if (curse_me)
	{
		add_ability (a_ptr);
		add_ability (a_ptr);
		do_curse (a_ptr);
		do_curse (a_ptr);
		do_curse (a_ptr);
		remove_contradictory (a_ptr);
		ap = artifact_power (a_idx, FALSE);
	}

	else
	{
		/* Select a random set of abilities which roughly matches the
		   original's in terms of overall power/usefulness. */
		for (tries = 0; tries < MAX_TRIES; tries++)
		{
			artifact_type a_old;

			/* Copy artifact info temporarily. */
			a_old = *a_ptr;
			add_ability (a_ptr);
			ap = artifact_power (a_idx, FALSE);
			if (ap > (power * 11) / 10 + 1)
			{	/* too powerful -- put it back */
				*a_ptr = a_old;
				continue;
			}

			else if (ap >= (power * 9) / 10)	/* just right */
			{
				break;
			}

			/* Stop if we're going negative, so we don't overload
			   the artifact with great powers to compensate. */
			else if ((ap < 0) && (ap < (-(power * 1)) / 10))
			{
				break;
			}
		}		/* end of power selection */

		if (aggravate_me)
		{
			a_ptr->flags3 |= TR3_AGGRAVATE;
			remove_contradictory (a_ptr);
			ap = artifact_power (a_idx, FALSE);
		}
	}

	a_ptr->cost = ap * (s32b)1000;

	if (a_ptr->cost < 0) a_ptr->cost = 0;

#if 0
	/* One last hack: if the artifact is very powerful, raise the rarity.
	   This compensates for artifacts like (original) Bladeturner, which
	   have low artifact rarities but came from extremely-rare base
	   kinds. */
	if ((ap > 0) && ((ap / 8) > a_ptr->rarity))
		a_ptr->rarity = ap / 8;
#endif 0

	if (activates) a_ptr->flags3 |= TR3_ACTIVATE;
	if (a_idx < ART_MIN_NORMAL) a_ptr->flags3 |= TR3_INSTA_ART;

	/* Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	   too lazy to find out which ones need it and which ones don't. */
	if (a_ptr->pval)
		a_ptr->flags3 |= TR3_HIDE_TYPE;

	return 0;
}

/* Randomly select a base item type (tval,sval).  Assign the various fields
   corresponding to that choice. */
void choose_item (int a_idx, u32b activates)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int tval, sval;
	object_kind *k_ptr;
	int r;
	s16b k_idx, r2;
	byte target_level;

	/* Look up the original artifact's base object kind to get level and
	   rarity information to supplement the artifact level/rarity.  As a
	   degenerate case consider Bladeturner, which has artifact lvl/rar
	   of only 95/3, but which is based on an object with 110/64! */
	k_idx = lookup_kind (a_ptr->tval, a_ptr->sval);
	k_ptr = &k_info[k_idx];
	target_level = k_ptr->level;

	/* Add base object kind's rarity to artifact rarity.  Later we will
	   subtract the new object kind's rarity. */
	a_ptr->rarity += k_ptr->chance[0];

	/* Pick a category (tval) of weapon randomly.  Within each tval, roll
	   an sval (specific item) based on the target level.  The number we
	   roll should be a bell curve.  The mean and standard variation of the
	   bell curve are based on the target level; the distribution of
	   kinds versus the bell curve is hand-tweaked. :-( */
	r = rand_int (100);
	if (r < 5)
	{
		/* Create a missile weapon. */
		tval = TV_BOW;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 3) sval = SV_SLING;
		else if (r2 < 10) sval = SV_SHORT_BOW;
		else if (r2 < 30) sval = SV_LONG_BOW;
		else if (r2 < 45) sval = SV_LIGHT_XBOW;
		else sval = SV_HEAVY_XBOW;
	}
	else if (r < 9)
	{
		/* Create a digging tool. */
		tval = TV_DIGGING;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 15) sval = SV_SHOVEL;
		else if (r2 < 30) sval = SV_PICK;
		else if (r2 < 60) sval = SV_GNOMISH_SHOVEL;
		else if (r2 < 90) sval = SV_ORCISH_PICK;
		else if (r2 < 120) sval = SV_DWARVEN_SHOVEL;
		else sval = SV_DWARVEN_PICK;
	}
	else if (r < 19)
	{
		/* Create a "blunt" weapon. */
		tval = TV_HAFTED;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 6) sval = SV_WHIP;
		else if (r2 < 12) sval = SV_MACE;
		else if (r2 < 20) sval = SV_WAR_HAMMER;
		else if (r2 < 30) sval = SV_QUARTERSTAFF;
		else if (r2 < 34) sval = SV_LUCERN_HAMMER;
		else if (r2 < 38) sval = SV_MORNING_STAR;
		else if (r2 < 45) sval = SV_FLAIL;
		else if (r2 < 55) sval = SV_LEAD_FILLED_MACE;
		else if (r2 < 80) sval = SV_BALL_AND_CHAIN;
		else if (r2 < 120) sval = SV_TWO_HANDED_FLAIL;
		else sval = SV_MACE_OF_DISRUPTION;
	}
	else if (r < 33)
	{
		/* Create a long, sharp-edged weapon. */
		tval = TV_SWORD;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 0) sval = SV_BROKEN_DAGGER;
		else if (r2 < 1) sval = SV_BROKEN_SWORD;
		else if (r2 < 5) sval = SV_DAGGER;
		else if (r2 < 9) sval = SV_MAIN_GAUCHE;
		else if (r2 < 10) sval = SV_RAPIER;	/* or at least pointy ;-) */
		else if (r2 < 12) sval = SV_SMALL_SWORD;
		else if (r2 < 14) sval = SV_SHORT_SWORD;
		else if (r2 < 16) sval = SV_SABRE;
		else if (r2 < 18) sval = SV_CUTLASS;
		else if (r2 < 20) sval = SV_TULWAR;
		else if (r2 < 23) sval = SV_BROAD_SWORD;
		else if (r2 < 26) sval = SV_LONG_SWORD;
		else if (r2 < 30) sval = SV_SCIMITAR;
		else if (r2 < 45) sval = SV_BASTARD_SWORD;
		else if (r2 < 60) sval = SV_KATANA;
		else if (r2 < 90) sval = SV_TWO_HANDED_SWORD;
		else if (r2 < 120) sval = SV_EXECUTIONERS_SWORD;
		else sval = SV_BLADE_OF_CHAOS;
	}
	else if (r < 42)
	{
		/* Create a weapon that's not blunt or sword-shaped. */
		tval = TV_POLEARM;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 12) sval = SV_SPEAR;
		else if (r2 < 20) sval = SV_TRIDENT;
		else if (r2 < 27) sval = SV_LANCE;
		else if (r2 < 35) sval = SV_AWL_PIKE;
		else if (r2 < 45) sval = SV_PIKE;
		else if (r2 < 50) sval = SV_BEAKED_AXE;
		else if (r2 < 55) sval = SV_BROAD_AXE;
		else if (r2 < 60) sval = SV_BATTLE_AXE;
		else if (r2 < 65) sval = SV_GLAIVE;
		else if (r2 < 80) sval = SV_HALBERD;
		else if (r2 < 120) sval = SV_GREAT_AXE;
		else if (r2 < 128) sval = SV_SCYTHE;
		else if (r2 < 135) sval = SV_LOCHABER_AXE;
		else sval = SV_SCYTHE_OF_SLICING;
	}
	else if (r < 64)
	{
		/* Create light or hard body armor. */
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 45) tval = TV_SOFT_ARMOR; else tval = TV_HARD_ARMOR;

		/* Soft stuff. */
		if (r2 < 0) sval = SV_FILTHY_RAG;
		else if (r2 < 5) sval = SV_ROBE;
		else if (r2 < 10) sval = SV_SOFT_LEATHER_ARMOR;
		else if (r2 < 15) sval = SV_SOFT_STUDDED_LEATHER;
		else if (r2 < 20) sval = SV_HARD_LEATHER_ARMOR;
		else if (r2 < 30) sval = SV_HARD_STUDDED_LEATHER;
		else if (r2 < 45) sval = SV_LEATHER_SCALE_MAIL;

		/* Hard stuff. */
		else if (r2 < 55) sval = SV_RUSTY_CHAIN_MAIL;
		else if (r2 < 65) sval = SV_METAL_SCALE_MAIL;
		else if (r2 < 75) sval = SV_CHAIN_MAIL;
		else if (r2 < 85) sval = SV_AUGMENTED_CHAIN_MAIL;
		else if (r2 < 90) sval = SV_DOUBLE_CHAIN_MAIL;
		else if (r2 < 97) sval = SV_BAR_CHAIN_MAIL;
		else if (r2 < 105) sval = SV_METAL_BRIGANDINE_ARMOUR;
		else if (r2 < 115) sval = SV_PARTIAL_PLATE_ARMOUR;
		else if (r2 < 125) sval = SV_METAL_LAMELLAR_ARMOUR;
		else if (r2 < 135) sval = SV_FULL_PLATE_ARMOUR;
		else if (r2 < 140) sval = SV_RIBBED_PLATE_ARMOUR;
		else if (r2 < 150) sval = SV_MITHRIL_CHAIN_MAIL;
		else if (r2 < 170) sval = SV_MITHRIL_PLATE_MAIL;
		else sval = SV_ADAMANTITE_PLATE_MAIL;
	}
	else if (r < 71)
	{
		/* Make shoes. */
		tval = TV_BOOTS;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 9) sval = SV_PAIR_OF_SOFT_LEATHER_BOOTS;
		else if (r2 < 15) sval = SV_PAIR_OF_HARD_LEATHER_BOOTS;
		else sval = SV_PAIR_OF_METAL_SHOD_BOOTS;
	}
	else if (r < 78)
	{
		/* Make gloves. */
		tval = TV_GLOVES;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 10) sval = SV_SET_OF_LEATHER_GLOVES;
		else if (r2 < 30) sval = SV_SET_OF_GAUNTLETS;
		else sval = SV_SET_OF_CESTI;
	}
	else if (r < 87)
	{
		/* Make headgear. */
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 50) tval = TV_HELM; else tval = TV_CROWN;

		if (r2 < 9) sval = SV_HARD_LEATHER_CAP;
		else if (r2 < 20) sval = SV_METAL_CAP;
		else if (r2 < 40) sval = SV_IRON_HELM;
		else if (r2 < 50) sval = SV_STEEL_HELM;

		else if (r2 < 60) sval = SV_IRON_CROWN;
		else if (r2 < 90) sval = SV_GOLDEN_CROWN;
		else sval = SV_JEWELED_CROWN;
	}
	else if (r < 94)
	{
		/* Make a shield. */
		tval = TV_SHIELD;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 9) sval = SV_SMALL_LEATHER_SHIELD;
		else if (r2 < 20) sval = SV_SMALL_METAL_SHIELD;
		else if (r2 < 40) sval = SV_LARGE_LEATHER_SHIELD;
		else if (r2 < 60) sval = SV_LARGE_METAL_SHIELD;
		else sval = SV_SHIELD_OF_DEFLECTION;
	}
	else
	{
		/* Make a cloak. */
		tval = TV_CLOAK;
		r2 = Rand_normal (target_level * 2, target_level);
		if (r2 < 90) sval = SV_CLOAK;
		else sval = SV_SHADOW_CLOAK;
	}

	k_idx = lookup_kind (tval, sval);
	k_ptr = &k_info[k_idx];
	kinds[a_idx] = k_idx;

	/* Subtract the new object kind's rarity (see above).  We can't
	   blindly subtract, because a_ptr->rarity is a byte. */
	if (a_ptr->rarity <= k_ptr->chance[0])
		a_ptr->rarity = 1;
	else
		a_ptr->rarity -= k_ptr->chance[0];

	a_ptr->tval = k_ptr->tval;
	a_ptr->sval = k_ptr->sval;
	a_ptr->pval = k_ptr->pval;
	a_ptr->to_h = k_ptr->to_h;
	a_ptr->to_d = k_ptr->to_d;
	a_ptr->to_a = k_ptr->to_a;
	a_ptr->ac = k_ptr->ac;
	a_ptr->dd = k_ptr->dd;
	a_ptr->ds = k_ptr->ds;
	a_ptr->weight = k_ptr->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;
	a_ptr->flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
			   TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	/* Assign basic stats to the artifact based on its artifact level. */
	switch (a_ptr->tval)
	{
		case TV_BOW: case TV_DIGGING: case TV_HAFTED:
		case TV_SWORD: case TV_POLEARM:
			a_ptr->to_h += a_ptr->level / 10 + rand_int (4) + rand_int (4);
			a_ptr->to_d += a_ptr->level / 10 + rand_int (4) + rand_int ((a_ptr->dd * a_ptr->ds) / 2 + 1);
			break;
		case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
		case TV_SHIELD: case TV_CLOAK: case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
			a_ptr->to_a += a_ptr->level / 10 + a_ptr->ac / 3 + rand_int (8);
			if (a_ptr->to_a < 10) a_ptr->to_a += 2 + rand_int (4) + rand_int (4);
			/*
			 * Make sure armor gets some resists!  Hard body armor
			 * is generally high-level stuff, with good ac and
			 * to_a.  That sucks up all the points....
			 */
			switch (a_ptr->tval)
			{
			case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_RES_ACID;
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_RES_ELEC;
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_RES_COLD;
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_RES_FIRE;
				break;
			}
			break;
	}
}

/* Randomly select an extra ability to be added to the artifact in question.
   This function is way too large. */
void add_ability (artifact_type *a_ptr)
{
	int r;

	r = rand_int (10);
	if (r < 5)		/* Pick something dependent on item type. */
	{
		r = rand_int (100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				if (r < 15)
				{
					a_ptr->flags1 |= TR1_SHOTS;
					do_pval (a_ptr);
				}
				else if (r < 35)
				{
					a_ptr->flags1 |= TR1_MIGHT;
					do_pval (a_ptr);
				}
				else if (r < 65) a_ptr->to_h += 2 + rand_int (2);
				else a_ptr->to_d += 2 + rand_int (3);

				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 4)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval (a_ptr);
					if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_WIS;
					if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
						a_ptr->flags3 |= TR3_BLESSED;
				}
				else if (r < 7)
				{
					a_ptr->flags1 |= TR1_BRAND_ACID;
					if (rand_int (4) > 0) a_ptr->flags2 |= TR2_RES_ACID;
				}
				else if (r < 10)
				{
					a_ptr->flags1 |= TR1_BRAND_ELEC;
					if (rand_int (4) > 0) a_ptr->flags2 |= TR2_RES_ELEC;
				}
				else if (r < 15)
				{
					a_ptr->flags1 |= TR1_BRAND_FIRE;
					if (rand_int (4) > 0) a_ptr->flags2 |= TR2_RES_FIRE;
				}
				else if (r < 20)
				{
					a_ptr->flags1 |= TR1_BRAND_COLD;
					if (rand_int (4) > 0) a_ptr->flags2 |= TR2_RES_COLD;
				}
				else if (r < 28)
				{
					a_ptr->dd += 1 + rand_int (2) + rand_int (2);
					if (a_ptr->dd > 9) a_ptr->dd = 9;
				}
				else if (r < 31) a_ptr->flags1 |= TR1_KILL_DRAGON;
				else if (r < 35) a_ptr->flags1 |= TR1_SLAY_DRAGON;
				else if (r < 40) a_ptr->flags1 |= TR1_SLAY_EVIL;

				else if (r < 45) a_ptr->flags1 |= TR1_SLAY_ANIMAL;
				else if (r < 50)
				{
					a_ptr->flags1 |= TR1_SLAY_UNDEAD;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_DEMON;
				}
				else if (r < 54)
				{
					a_ptr->flags1 |= TR1_SLAY_DEMON;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_UNDEAD;
				}
				else if (r < 59)
				{
					a_ptr->flags1 |= TR1_SLAY_ORC;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_TROLL;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_GIANT;
				}
				else if (r < 63)
				{
					a_ptr->flags1 |= TR1_SLAY_TROLL;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_ORC;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_GIANT;
				}
				else if (r < 67)
				{
					a_ptr->flags1 |= TR1_SLAY_GIANT;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_ORC;
					if (rand_int (2) == 0) a_ptr->flags1 |= TR1_SLAY_TROLL;
				}
				else if (r < 72) a_ptr->flags3 |= TR3_SEE_INVIS;
				else if (r < 76)
				{
					if (a_ptr->pval < 0) break;
					a_ptr->flags1 |= TR1_BLOWS;
					do_pval (a_ptr);
				}
				else if (r < 89)
				{
					a_ptr->to_d += 3 + rand_int (4);
					a_ptr->to_h += 3 + rand_int (4);
				}
				else if (r < 92) a_ptr->to_a += 3 + rand_int (3);
				else if (r < 98)
					a_ptr->weight = (a_ptr->weight * 9) / 10;
				else
					if (a_ptr->tval != TV_DIGGING)
					{
						a_ptr->flags1 |= TR1_TUNNEL;
						do_pval (a_ptr);
					}

				break;
			}
			case TV_BOOTS:
			{
				if (r < 10) a_ptr->flags3 |= TR3_FEATHER;
				else if (r < 50) a_ptr->to_a += 2 + rand_int (4);
				else if (r < 80)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval (a_ptr);
				}
				else if (r < 90)
				{
					a_ptr->flags1 |= TR1_SPEED;
					if (a_ptr->pval < 0) break;
					if (a_ptr->pval == 0) a_ptr->pval = 3 + rand_int (8);
					else if (rand_int (2) == 0) a_ptr->pval++;
				}
				else a_ptr->weight = (a_ptr->weight * 9) / 10;
				break;
			}
			case TV_GLOVES:
			{
				if (r < 25) a_ptr->flags3 |= TR3_FREE_ACT;
				else if (r < 50)
				{
					a_ptr->flags1 |= TR1_DEX;
					do_pval (a_ptr);
				}
				else if (r < 75) a_ptr->to_a += 3 + rand_int (3);
				else
				{
					a_ptr->to_h += 2 + rand_int (3);
					a_ptr->to_d += 2 + rand_int (3);
					a_ptr->flags3 |= TR3_SHOW_MODS;
				}
				break;
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r < 20) a_ptr->flags2 |= TR2_RES_BLIND;
				else if (r < 45) a_ptr->flags3 |= TR3_TELEPATHY;
				else if (r < 65) a_ptr->flags3 |= TR3_SEE_INVIS;
				else if (r < 75)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval (a_ptr);
				}
				else if (r < 85)
				{
					a_ptr->flags1 |= TR1_INT;
					do_pval (a_ptr);
				}
				else a_ptr->to_a += 3 + rand_int (3);
				break;
			}
			case TV_SHIELD:
			{
				if (r < 20) a_ptr->flags2 |= TR2_RES_ACID;
				else if (r < 40) a_ptr->flags2 |= TR2_RES_ELEC;
				else if (r < 60) a_ptr->flags2 |= TR2_RES_FIRE;
				else if (r < 80) a_ptr->flags2 |= TR2_RES_COLD;
				else a_ptr->to_a += 3 + rand_int (3);
				break;
			}
			case TV_CLOAK:
			{
				if (r < 50)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval (a_ptr);
				}
				else a_ptr->to_a += 3 + rand_int (3);
				break;
			}
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			{
				if (r < 8)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval (a_ptr);
				}
				else if (r < 16) a_ptr->flags3 |= TR3_HOLD_LIFE;
				else if (r < 22)
				{
					a_ptr->flags1 |= TR1_CON;
					do_pval (a_ptr);
					if (rand_int (2) == 0)
						a_ptr->flags2 |= TR2_SUST_CON;
				}
				else if (r < 34) a_ptr->flags2 |= TR2_RES_ACID;
				else if (r < 46) a_ptr->flags2 |= TR2_RES_ELEC;
				else if (r < 58) a_ptr->flags2 |= TR2_RES_FIRE;
				else if (r < 70) a_ptr->flags2 |= TR2_RES_COLD;
				else if (r < 80)
					a_ptr->weight = (a_ptr->weight * 9) / 10;
				else a_ptr->to_a += 3 + rand_int (3);
				break;
			}
		}
	}
	else			/* Pick something universally useful. */
	{
		r = rand_int (43);
		switch (r)
		{
			case 0:
				a_ptr->flags1 |= TR1_STR;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_STR;
				break;
			case 1:
				a_ptr->flags1 |= TR1_INT;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_INT;
				break;
			case 2:
				a_ptr->flags1 |= TR1_WIS;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_WIS;
				if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
					a_ptr->flags3 |= TR3_BLESSED;
				break;
			case 3:
				a_ptr->flags1 |= TR1_DEX;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_DEX;
				break;
			case 4:
				a_ptr->flags1 |= TR1_CON;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_CON;
				break;
			case 5:
				a_ptr->flags1 |= TR1_CHR;
				do_pval (a_ptr);
				if (rand_int (2) == 0) a_ptr->flags2 |= TR2_SUST_CHR;
				break;

			case 6:
				a_ptr->flags1 |= TR1_STEALTH;
				do_pval (a_ptr);
				break;
			case 7:
				a_ptr->flags1 |= TR1_SEARCH;
				do_pval (a_ptr);
				break;
			case 8:
				a_ptr->flags1 |= TR1_INFRA;
				do_pval (a_ptr);
				break;
			case 9:
				a_ptr->flags1 |= TR1_SPEED;
				if (a_ptr->pval == 0) a_ptr->pval = 3 + rand_int (3);
				else do_pval (a_ptr);
				break;

			case 10:
				a_ptr->flags2 |= TR2_SUST_STR;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_STR;
					do_pval (a_ptr);
				}
				break;
			case 11:
				a_ptr->flags2 |= TR2_SUST_INT;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_INT;
					do_pval (a_ptr);
				}
				break;
			case 12:
				a_ptr->flags2 |= TR2_SUST_WIS;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval (a_ptr);
					if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
						a_ptr->flags3 |= TR3_BLESSED;
				}
				break;
			case 13:
				a_ptr->flags2 |= TR2_SUST_DEX;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_DEX;
					do_pval (a_ptr);
				}
				break;
			case 14:
				a_ptr->flags2 |= TR2_SUST_CON;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_CON;
					do_pval (a_ptr);
				}
				break;
			case 15:
				a_ptr->flags2 |= TR2_SUST_CHR;
				if (rand_int (2) == 0)
				{
					a_ptr->flags1 |= TR1_CHR;
					do_pval (a_ptr);
				}
				break;

			case 16:
			{
				if (rand_int (3) == 0) a_ptr->flags2 |= TR2_IM_ACID;
				break;
			}
			case 17:
			{
				if (rand_int (3) == 0) a_ptr->flags2 |= TR2_IM_ELEC;
				break;
			}
			case 18:
			{
				if (rand_int (4) == 0) a_ptr->flags2 |= TR2_IM_FIRE;
				break;
			}
			case 19:
			{
				if (rand_int (3) == 0) a_ptr->flags2 |= TR2_IM_COLD;
				break;
			}
			case 20: a_ptr->flags3 |= TR3_FREE_ACT; break;
			case 21: a_ptr->flags3 |= TR3_HOLD_LIFE; break;
			case 22: a_ptr->flags2 |= TR2_RES_ACID; break;
			case 23: a_ptr->flags2 |= TR2_RES_ELEC; break;
			case 24: a_ptr->flags2 |= TR2_RES_FIRE; break;
			case 25: a_ptr->flags2 |= TR2_RES_COLD; break;

			case 26: a_ptr->flags2 |= TR2_RES_POIS; break;
			case 27: a_ptr->flags2 |= TR2_RES_LITE; break;
			case 28: a_ptr->flags2 |= TR2_RES_DARK; break;
			case 29: a_ptr->flags2 |= TR2_RES_BLIND; break;
			case 30: a_ptr->flags2 |= TR2_RES_CONFU; break;
			case 31: a_ptr->flags2 |= TR2_RES_SOUND; break;
			case 32: a_ptr->flags2 |= TR2_RES_SHARD; break;
			case 33:
				if (rand_int (2) == 0)
					a_ptr->flags2 |= TR2_RES_NETHR;
				break;
			case 34: a_ptr->flags2 |= TR2_RES_NEXUS; break;
			case 35: a_ptr->flags2 |= TR2_RES_CHAOS; break;
			case 36:
				if (rand_int (2) == 0)
					a_ptr->flags2 |= TR2_RES_DISEN;
				break;
			case 37: a_ptr->flags3 |= TR3_FEATHER; break;
			case 38: a_ptr->flags3 |= TR3_LITE; break;
			case 39: a_ptr->flags3 |= TR3_SEE_INVIS; break;
			case 40:
				if (rand_int (3) == 0)
					a_ptr->flags3 |= TR3_TELEPATHY;
				break;
			case 41: a_ptr->flags3 |= TR3_SLOW_DIGEST; break;

			case 42:
				a_ptr->flags3 |= TR3_REGEN; break;
		}
	}

	/* Now remove contradictory or redundant powers. */
	remove_contradictory (a_ptr);
}

void remove_contradictory (artifact_type *a_ptr)
{
	if (a_ptr->flags3 & TR3_AGGRAVATE) a_ptr->flags1 &= ~(TR1_STEALTH);
	if (a_ptr->flags2 & TR2_IM_ACID) a_ptr->flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->flags2 & TR2_IM_ELEC) a_ptr->flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->flags2 & TR2_IM_FIRE) a_ptr->flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->flags2 & TR2_IM_COLD) a_ptr->flags2 &= ~(TR2_RES_COLD);
	if (a_ptr->flags2 & TR2_RES_CHAOS) a_ptr->flags2 &= ~(TR2_RES_CONFU);
	if (a_ptr->pval < 0)
	{
		if (a_ptr->flags1 & TR1_STR) a_ptr->flags2 &= ~(TR2_SUST_STR);
		if (a_ptr->flags1 & TR1_INT) a_ptr->flags2 &= ~(TR2_SUST_INT);
		if (a_ptr->flags1 & TR1_WIS) a_ptr->flags2 &= ~(TR2_SUST_WIS);
		if (a_ptr->flags1 & TR1_DEX) a_ptr->flags2 &= ~(TR2_SUST_DEX);
		if (a_ptr->flags1 & TR1_CON) a_ptr->flags2 &= ~(TR2_SUST_CON);
		if (a_ptr->flags1 & TR1_CHR) a_ptr->flags2 &= ~(TR2_SUST_CHR);
		a_ptr->flags1 &= ~(TR1_BLOWS);
	}
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) a_ptr->flags3 &= ~(TR3_BLESSED);
	if (a_ptr->flags1 & TR1_KILL_DRAGON) a_ptr->flags1 &= ~(TR1_SLAY_DRAGON);
	if (a_ptr->flags3 & TR3_DRAIN_EXP) a_ptr->flags3 &= ~(TR3_HOLD_LIFE);
}

/* We've just added an ability which uses the pval bonus.  Make sure it's
   not zero.  If it's currently negative, leave it negative (heh heh). */
void do_pval (artifact_type *a_ptr)
{
	if (a_ptr->pval == 0) a_ptr->pval = 1 + rand_int (3);
	else if (a_ptr->pval < 0)
	{
		if (rand_int (2) == 0) a_ptr->pval--;
	}
	else if (rand_int (3) > 0) a_ptr->pval++;
}

/* Make it bad, or if it's already bad, make it worse! */
void do_curse (artifact_type *a_ptr)
{
	if (rand_int (3) == 0)
		a_ptr->flags3 |= TR3_AGGRAVATE;
	if (rand_int (5) == 0)
		a_ptr->flags3 |= TR3_DRAIN_EXP;
	if (rand_int (7) == 0)
		a_ptr->flags3 |= TR3_TELEPORT;

	if ((a_ptr->pval > 0) && (rand_int (2) == 0))
		a_ptr->pval = -a_ptr->pval;
	if ((a_ptr->to_a > 0) && (rand_int (2) == 0))
		a_ptr->to_a = -a_ptr->to_a;
	if ((a_ptr->to_h > 0) && (rand_int (2) == 0))
		a_ptr->to_h = -a_ptr->to_h;
	if ((a_ptr->to_d > 0) && (rand_int (4) == 0))
		a_ptr->to_d = -a_ptr->to_d;

	if (a_ptr->flags3 & TR3_LIGHT_CURSE)
	{
		if (rand_int (2) == 0) a_ptr->flags3 |= TR3_HEAVY_CURSE;
		return;
	}

	a_ptr->flags3 |= TR3_LIGHT_CURSE;
	if (rand_int (4) == 0) a_ptr->flags3 |= TR3_HEAVY_CURSE;
}

/* Evaluate the artifact's overall power level. */
s32b artifact_power (int a_idx, bool cannot_use_kind_cache)
{
	const artifact_type *a_ptr = &a_info[a_idx];
	s32b p = 0;
	s16b k_idx;
	object_kind *k_ptr = NULL;	/* silence the warning */
	int immunities = 0;

	/* Start with a "power" rating derived from the base item's level. */
	if (a_idx >= ART_MIN_NORMAL)
	{
		if (cannot_use_kind_cache)
			k_idx = lookup_kind (a_ptr->tval, a_ptr->sval);
		else
			k_idx = kinds [a_idx];

		if (k_idx)
		{
			k_ptr = &k_info[k_idx];
			p = (k_ptr->level + 7) / 8;
		}
		/* Otherwise just forget it and use 0. ;-) */
	}

	/* Evaluate certain abilities based on type of object. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			p += (a_ptr->to_d + sign (a_ptr->to_d)) / 2;
			mult = bow_multiplier (a_ptr->sval);
			if (a_ptr->flags1 & TR1_MIGHT)
			{
				if (a_ptr->pval > 3)
				{
					p += 20000;	/* inhibit */
					mult = 1;	/* don't overflow */
				}
				else
					mult += a_ptr->pval;
			}
			p *= mult;
			if (a_ptr->flags1 & TR1_SHOTS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p *= (2 * a_ptr->pval);
			}
			p += (a_ptr->to_h + 3 * sign (a_ptr->to_h)) / 4;
			if (a_ptr->weight < k_ptr->weight) p++;
			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			p += (a_ptr->dd * a_ptr->ds + 1) / 2;
			if (a_ptr->flags1 & TR1_SLAY_EVIL) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_DRAGON) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_SLAY_ANIMAL) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_UNDEAD) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DRAGON) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DEMON) p = (p * 5) / 4;
			if (a_ptr->flags1 & TR1_SLAY_TROLL) p = (p * 5) / 4;
			if (a_ptr->flags1 & TR1_SLAY_ORC) p = (p * 5) / 4;
			if (a_ptr->flags1 & TR1_SLAY_GIANT) p = (p * 6) / 5;

			if (a_ptr->flags1 & TR1_BRAND_ACID) p = p * 2;
			if (a_ptr->flags1 & TR1_BRAND_ELEC) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_BRAND_FIRE) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_BRAND_COLD) p = (p * 4) / 3;

			p += (a_ptr->to_d + 2 * sign (a_ptr->to_d)) / 3;
			if (a_ptr->to_d > 15) p += (a_ptr->to_d - 14) / 2;

			if (a_ptr->flags1 & TR1_BLOWS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p = (p * 6) / (4 - a_ptr->pval);
			}

			if ((a_ptr->flags1 & TR1_TUNNEL) &&
			    (a_ptr->tval != TV_DIGGING))
				p += a_ptr->pval * 3;

			p += (a_ptr->to_h + 3 * sign (a_ptr->to_h)) / 4;

			/* Remember, weight is in 0.1 lb. units. */
			if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 20;

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			p += (a_ptr->ac + 4 * sign (a_ptr->ac)) / 5;
			p += (a_ptr->to_h + sign (a_ptr->to_h)) / 2;
			p += (a_ptr->to_d + sign (a_ptr->to_d)) / 2;
			if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 30;
			break;
		}
		case TV_LITE:
		{
			p += 10;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			p += 20;
			break;
		}
	}

	/* Other abilities are evaluated independent of the object type. */
	p += (a_ptr->to_a + 3 * sign (a_ptr->to_a)) / 4;
	if (a_ptr->to_a > 20) p += (a_ptr->to_a - 19) / 2;
	if (a_ptr->to_a > 30) p += (a_ptr->to_a - 29) / 2;
	if (a_ptr->to_a > 40) p += 20000;	/* inhibit */

	if (a_ptr->pval > 0)
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval * a_ptr->pval;
	}
	else if (a_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval;
	}
	if (a_ptr->flags1 & TR1_CHR) p += a_ptr->pval;
	if (a_ptr->flags1 & TR1_INFRA) p += (a_ptr->pval + sign (a_ptr->pval)) / 2;
	if (a_ptr->flags1 & TR1_SPEED) p += (a_ptr->pval * 3) / 2;

	if (a_ptr->flags2 & TR2_SUST_STR) p += 6;
	if (a_ptr->flags2 & TR2_SUST_INT) p += 4;
	if (a_ptr->flags2 & TR2_SUST_WIS) p += 4;
	if (a_ptr->flags2 & TR2_SUST_DEX) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CON) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CHR) p += 1;
	if (a_ptr->flags2 & TR2_IM_ACID)
	{
		p += 20;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_ELEC)
	{
		p += 24;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_FIRE)
	{
		p += 36;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_COLD)
	{
		p += 24;
		immunities++;
	}
	if (immunities > 1) p += 16;
	if (immunities > 2) p += 16;
	if (immunities > 3) p += 20000;		/* inhibit */
	if (a_ptr->flags3 & TR3_FREE_ACT) p += 8;
	if (a_ptr->flags3 & TR3_HOLD_LIFE) p += 10;
	if (a_ptr->flags2 & TR2_RES_ACID) p += 6;
	if (a_ptr->flags2 & TR2_RES_ELEC) p += 6;
	if (a_ptr->flags2 & TR2_RES_FIRE) p += 6;
	if (a_ptr->flags2 & TR2_RES_COLD) p += 6;
	if (a_ptr->flags2 & TR2_RES_POIS) p += 12;
	if (a_ptr->flags2 & TR2_RES_LITE) p += 8;
	if (a_ptr->flags2 & TR2_RES_DARK) p += 10;
	if (a_ptr->flags2 & TR2_RES_BLIND) p += 10;
	if (a_ptr->flags2 & TR2_RES_CONFU) p += 8;
	if (a_ptr->flags2 & TR2_RES_SOUND) p += 10;
	if (a_ptr->flags2 & TR2_RES_SHARD) p += 8;
	if (a_ptr->flags2 & TR2_RES_NETHR) p += 12;
	if (a_ptr->flags2 & TR2_RES_NEXUS) p += 10;
	if (a_ptr->flags2 & TR2_RES_CHAOS) p += 12;
	if (a_ptr->flags2 & TR2_RES_DISEN) p += 12;

	if (a_ptr->flags3 & TR3_FEATHER) p += 2;
	if (a_ptr->flags3 & TR3_LITE) p += 2;
	if (a_ptr->flags3 & TR3_SEE_INVIS) p += 8;
	if (a_ptr->flags3 & TR3_TELEPATHY) p += 20;
	if (a_ptr->flags3 & TR3_SLOW_DIGEST) p += 4;
	if (a_ptr->flags3 & TR3_REGEN) p += 8;
	if (a_ptr->flags3 & TR3_TELEPORT) p -= 20;
	if (a_ptr->flags3 & TR3_DRAIN_EXP) p -= 16;
	if (a_ptr->flags3 & TR3_AGGRAVATE) p -= 8;
	if (a_ptr->flags3 & TR3_BLESSED) p += 4;
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) p -= 4;
	if (a_ptr->flags3 & TR3_HEAVY_CURSE) p -= 20;
/*	if (a_ptr->flags3 & TR3_PERMA_CURSE) p -= 40; */

	return p;
}

/* Return nonzero if the whole set of random artifacts meets certain
   criteria.  Return 0 if we fail to meet those criteria (which will
   restart the whole process). */
int artifacts_acceptable (void)
{
	int swords = 5, polearms = 5, blunts = 5, bows = 3;
	int bodies = 5, shields = 3, cloaks = 3, hats = 4;
	int gloves = 4, boots = 4;
	int i;

	for (i = ART_MIN_NORMAL; i < MAX_A_IDX; i++)
	{
		switch (a_info[i].tval)
		{
			case TV_SWORD:
				swords--; break;
			case TV_POLEARM:
				polearms--; break;
			case TV_HAFTED:
				blunts--; break;
			case TV_BOW:
				bows--; break;
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
				bodies--; break;
			case TV_SHIELD:
				shields--; break;
			case TV_CLOAK:
				cloaks--; break;
			case TV_HELM:
			case TV_CROWN:
				hats--; break;
			case TV_GLOVES:
				gloves--; break;
			case TV_BOOTS:
				boots--; break;
		}
	}

	if (swords > 0 || polearms > 0 || blunts > 0 || bows > 0 ||
	    bodies > 0 || shields > 0 || cloaks > 0 || hats > 0 ||
	    gloves > 0 || boots > 0)
	{
		if (randart_verbose)
		{
			char types [256];
			sprintf (types, "%s%s%s%s%s%s%s%s%s%s",
				swords > 0 ? " swords" : "",
				polearms > 0 ? " polearms" : "",
				blunts > 0 ? " blunts" : "",
				bows > 0 ? " bows" : "",
				bodies > 0 ? " body-armors" : "",
				shields > 0 ? " shields" : "",
				cloaks > 0 ? " cloaks" : "",
				hats > 0 ? " hats" : "",
				gloves > 0 ? " gloves" : "",
				boots > 0 ? " boots" : "");
			msg_format ("Restarting generation process: not enough%s\n",
				types);
		}
		return 0;
	}
	else
	{
		return 1;
	}
}

/* Calculate the multiplier we'll get with a given bow type.  This is done
   differently in 2.8.2 than it was in 2.8.1. */
int bow_multiplier (int sval)
{
	switch (sval)
	{
		case SV_SLING:
		case SV_SHORT_BOW: return 2;

		case SV_LONG_BOW:
		case SV_LIGHT_XBOW: return 3;

		case SV_HEAVY_XBOW: return 4;

		default: msg_format ("Illegal bow sval %s\n", sval);
	}
	return 0;
}

char *my_strdup (const char *s)
{
	char *t = malloc (strlen (s) + 1);
	if (t) strcpy (t, s);
	return t;
}

#endif /* GJW_RANDART */

