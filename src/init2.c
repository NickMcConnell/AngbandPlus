#define INIT2_C
/* File: init2.c */

/* Purpose: Initialization (part 2) -BEN- */

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



#ifdef PRIVATE_USER_PATH

/*
 * Create an ".angband/" directory in the users home directory.
 *
 * ToDo: Add error handling.
 * ToDo: Only create the directories when actually writing files.
 */
static void create_user_dir(void)
{
	cptr ANGBAND_DIR_USER_LOC;

	/* Create the ~/.angband/ directory */
	my_mkdir(PRIVATE_USER_PATH, 0700);

	/* Build the path to the variant-specific sub-directory */
	ANGBAND_DIR_USER_LOC =
		string_make(format("%v", path_build_f2, PRIVATE_USER_PATH, GAME_NAME));

	/* Create the directory */
	switch (my_mkdir(ANGBAND_DIR_USER_LOC, 0700))
	{
		case FILE_ERROR_FILE_EXISTS:
		{
			/* Do nothing to a pre-existing directory. */
			break;
		}
		case FILE_ERROR_CANNOT_OPEN_FILE:
		{
			/* Something bad happened, so hope the old user dir is okay... */
			return;
		}
		case SUCCESS:
		{
			char from[1024], to[1024];
			/* New directory, so copy default user file to it.
			 * Maybe it should copy all pref files in ANGBAND_DIR_USER... */

			/* Build the paths. */
			strnfmt(from, 1024, "%v", path_build_f2, ANGBAND_DIR_USER,
				"user-loc.prf");
			strnfmt(to, 1024, "%v", path_build_f2, ANGBAND_DIR_USER_LOC,
				"user-loc.prf");

			/* Try to copy the file. */
			fd_copy(to, from);
		}
	}

	/* The system user directory will not be looked at again. */
	FREE(ANGBAND_DIR_USER);
	ANGBAND_DIR_USER = ANGBAND_DIR_USER_LOC;
}

#endif /* PRIVATE_USER_PATH */

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
void init_file_paths(cptr path)
{
	/*** Free everything ***/

	/* Free the sub-paths */
	FREE(ANGBAND_DIR_APEX);
	FREE(ANGBAND_DIR_BONE);
	FREE(ANGBAND_DIR_DATA);
	FREE(ANGBAND_DIR_EDIT);
	FREE(ANGBAND_DIR_FILE);
	FREE(ANGBAND_DIR_HELP);
	FREE(ANGBAND_DIR_INFO);
	FREE(ANGBAND_DIR_PREF);
	FREE(ANGBAND_DIR_SAVE);
	FREE(ANGBAND_DIR_USER);
	FREE(ANGBAND_DIR_XTRA);


	/*** Prepare the "path" ***/

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

	ANGBAND_DIR_APEX = string_make(format("%s%s", path, "apex"));
	ANGBAND_DIR_BONE = string_make(format("%s%s", path, "bone"));
	ANGBAND_DIR_DATA = string_make(format("%s%s", path, "data"));
	ANGBAND_DIR_EDIT = string_make(format("%s%s", path, "edit"));
	ANGBAND_DIR_FILE = string_make(format("%s%s", path, "file"));
	ANGBAND_DIR_HELP = string_make(format("%s%s", path, "help"));
	ANGBAND_DIR_INFO = string_make(format("%s%s", path, "info"));
	ANGBAND_DIR_PREF = string_make(format("%s%s", path, "pref"));
	ANGBAND_DIR_SAVE = string_make(format("%s%s", path, "save"));
	ANGBAND_DIR_USER = string_make(format("%s%s", path, "user"));
	ANGBAND_DIR_XTRA = string_make(format("%s%s", path, "xtra"));

#endif /* VM */
#ifdef PRIVATE_USER_PATH

	/* Change ANGBAND_DIR_USER to point to a local directory, copying files
	 * from the existing ANGBAND_DIR_USER if new. */
	create_user_dir();

#endif /* PRIVATE_USER_PATH */



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
			FREE(ANGBAND_DIR_DATA);

			/* Build a new path name */
			ANGBAND_DIR_DATA = string_make(format("data-%s", next));
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
	"too many arguments",
	"invalid spell frequency",
	"incorrect syntax",
};


#endif

#if defined(CHECK_MODIFICATION_TIME) && defined(HAS_STAT)

/*
 * Hack - put the SET_UID version of the check_modification_date() function
 * here.
 */
static errr check_modification_date(int fd, cptr template_file)
{
	char buf[1024];

	struct stat txt_stat, raw_stat;

	/* Build the filename */
	strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_EDIT, template_file);

	/* Access stats on text file */
	if (stat(buf, &txt_stat))
	{
		/* Error */
		return (-1);
	}

	/* Access stats on raw file */
	if (fstat(fd, &raw_stat))
	{
		/* Error */
		return (-1);
	}

	/* Ensure text file is not newer than raw file */
	if (txt_stat.st_mtime > raw_stat.st_mtime)
	{
		/* Reprocess text file */
		return (-1);
	}

	return (0);
}

#endif /* CHECK_MODIFICATION_TIME && HAS_STAT */

/*
 * A hook for a function which compares the modification date of a raw fd
 * to the text file from which it was derived.
 */
errr (*check_modification_date_hook)(int fd, cptr template_file) = 0;

/*** Initialize from binary image files ***/


/*
 * Initialize the header of an *_info.raw file.
 */
static void init_header_aux(header *head, int len, byte num,
	parse_info_txt_func parse, cptr name)
{
	/* Save the "version" */
	strcpy(head->version, GAME_VERSION);

	/* Save the "record" information */
	head->info_len = len;

	/* Save the name and number. */
	head->file_name = name;
	head->header_num = num;

#ifdef ALLOW_TEMPLATES
	head->parse_info_txt = parse;
#endif /* ALLOW_TEMPLATES */

	/* Set other fields to default values. */
	head->info_num = 0;
	head->info_size = 0;
	head->name_size = 0;
	head->text_size = 0;

	head->info_ptr = NULL;
	head->name_ptr = NULL;
	head->text_ptr = NULL;

	/* For z_info, the working array is the real array. */
	if (num == Z_HEAD)
	{
		C_MAKE(head->info_ptr, 1, maxima);
	}
}

/* Just because... */
#define init_header(W,X,Y,Z) init_header_aux(head,sizeof(W),X,Y,Z)

/*
 * Initialize a "*_info" array, by parsing a binary "image" file
 */
static errr init_info_raw(int fd, header *head)
{
	header test;

	/* Read and verify the header */
	if (fd_read(fd, (char*)(&test), sizeof(header)) ||
		(!streq(test.version, head->version)) ||
		(test.header_num != head->header_num) ||
		(test.info_len != head->info_len))
	{
		/* Error */
		return (-1);
	}

	/* Accept the header */
	head->info_num = test.info_num;
	head->info_size = test.info_size;
	head->name_size = test.name_size;
	head->text_size = test.text_size;


	/* z_head->info_ptr already points to the real array. */
	if (head->header_num != Z_HEAD)
	{
		/* Allocate the "*_info" array */
		C_MAKE(head->info_ptr, head->info_size, char);
	}

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
	msg_format("Error at line %d of '%s'.", error_line, filename);
	msg_format("Record %d contains a '%s' error.", error_idx, oops);
	msg_format("Parsing '%s'.", buf);
	message_flush();

	/* Quit */
	quit_fmt("Error in '%s' file.", filename);
}

#endif /* ALLOW_TEMPLATES */

/*
 * Find the name of the text file from which a header is read.
 *
 * Most are simply derived from the name of the raw file, but there are a
 * few exceptions.
 */
static cptr init_info_text_name(header *head)
{
	switch (head->header_num)
	{
		case EVENT_HEAD:
			return "r_info.txt";
		case D_HEAD: case T_HEAD: case Q_HEAD:
			return "d_info.txt";
		default:
			return format("%s.txt", head->file_name);
	}
}

/*
 * Initialize a "*_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static void init_info(header *head)
{
	cptr filename = head->file_name;

	char textname[13];

	int fd;

	errr err = 1;

	FILE *fp;

	/* General buffer */
	char buf[1024];


#ifdef ALLOW_TEMPLATES

	/* Find the text file name (should be in 8.3 format). */
	sprintf(textname, "%.12s", init_info_text_name(head));

	/*** Load the binary image file ***/

	if (~rebuild_raw & 1<<(head->header_num))
	{
		/* Build the filename */
		strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_DATA, format("%s.raw", filename));

		/* Attempt to open the "raw" file */
		fd = fd_open(buf, O_RDONLY);

		/* Process existing "raw" file */
		if (fd >= 0)
		{
#ifdef CHECK_MODIFICATION_TIME

			if (check_modification_date_hook)
			{
				err = (*check_modification_date_hook)(fd, textname);
			}

#endif /* CHECK_MODIFICATION_TIME */

			/* Attempt to parse the "raw" file */
			if (!err)
				err = init_info_raw(fd, head);

			/* Close it */
			fd_close(fd);
		}
	}

	/* Do we have to parse the *.txt file? */
	if (!err) return;

	/*** Reset the fake arrays ***/

	if (head->header_num != Z_HEAD)
	{
		head->info_ptr = C_WIPE(head->fake_info_ptr, z_info->fake_info_size, char);
		head->name_ptr = C_WIPE(head->fake_name_ptr, z_info->fake_name_size, char);
		head->text_ptr = C_WIPE(head->fake_text_ptr, z_info->fake_text_size, char);
	}
	else
	{
		head->info_ptr = C_WIPE(head->info_ptr, sizeof(maxima), char);
	}

	/*** Load the ascii template file ***/

	/* Build the filename and open the file. */
	fp = my_fopen_path(ANGBAND_DIR_EDIT, textname, "r");

	/* Parse it */
	if (!fp) quit(format("Cannot open '%s' file.", textname));

	/* Parse the file */
	err = init_info_txt(fp, buf, head);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err) display_parse_error(textname, err, buf);


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_DATA, format("%s.raw", filename));


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
			sprintf(why, "Cannot create the '%s' file!", buf);

			/* Crash and burn */
			quit(why);
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
		fd_write(fd, (cptr)head, sizeof(header));

		/* Dump the "*_info" array */
		fd_write(fd, head->info_ptr, head->info_size);

		/* Dump the "*_name" array */
		fd_write(fd, head->name_ptr, head->name_size);

		/* Dump the "*_text" array */
		fd_write(fd, head->text_ptr, head->text_size);

		/* Close */
		fd_close(fd);
	}

#endif /* ALLOW_TEMPLATES */


	/*** Load the binary image file ***/

	/* Build the filename */
	strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_DATA, format("%s.raw", filename));

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
}

static void init_u_info_final(void)
{
	int i,p_id;

	/* Check that there are enough entries for each p_id to cover
	 * the object kinds, with one extra per p_id for plain_descriptions. */
	for (p_id = 0; p_id < 256; p_id++)
	{
		s16b bal;

		for (i = bal = 0; i < z_info->k_max; i++)
		{
			if (k_info[i].u_idx == p_id) bal++;
		}
		for (i = 0; bal > 0 && i < z_info->u_max; i++)
		{
			unident_type *u_ptr = &u_info[i];
			if (u_ptr->p_id != p_id) continue;

			/* Unnamed objects need no extra elements. */
			if (u_ptr->name == 0) bal = 0;
			else bal--;
		}
		/* And if there aren't enough, complain. */
		if (bal > 0)
		{
			quit_fmt("Insufficient u_info entries with p_id %d: %d missing.", p_id, bal);
		}
	}
}

/*
 * Check that there is at least one shopkeeper available for each shop in the
 * game.
 */
static void init_s_info_final(void)
{
	byte *i;
	owner_type *s_ptr;
	town_type *t_ptr;
	for (t_ptr = town_defs; t_ptr < town_defs+MAX_TOWNS; t_ptr++)
	{
		for (i = t_ptr->store; i < t_ptr->store+MAX_STORES_PER_TOWN; i++)
		{
			/* Not a real shop. */
			if (*i == STORE_NONE) continue;

			for (s_ptr = owners; s_ptr < owners+z_info->owners; s_ptr++)
			{
				if (s_ptr->shop_type != *i) continue; /* Wrong type. */
				if (s_ptr->town != TOWN_NONE && s_ptr->town != t_ptr-town_defs)
					continue; /* Wrong town. */

				/* Acceptable. */
				goto next_store;
			}
			quit_fmt("Failed to find a shopkeeper for shop %d in %s.",
				*i, town_name+t_ptr->name);
next_store:
			continue;
		}
	}
}

/*
 * Put any function which needs to be called after an array is parsed here.
 */
static void init_x_final(int num)
{
	switch (num)
	{
		case U_HEAD:
		init_u_info_final();
		return;
		case S_HEAD:
		init_s_info_final();
		return;
	}
	return;
}

#ifdef ALLOW_TEMPLATES
#define IF_AT(X) X
#else /* ALLOW_TEMPLATES */
#define IF_AT(X) 0
#endif /* ALLOW_TEMPLATES */

#define init_x_info(title, type, parse, file, x_info, x_name, x_text, x_max, num) \
{ \
	note(format("[Initializing arrays... (%s)]", title)); \
	init_header(type, num, IF_AT(parse), file); \
	init_info(head); \
	x_info = head->info_ptr; \
	if (x_name != dummy) x_name = head->name_ptr; \
	if (x_text != dummy) x_text = head->text_ptr; \
	z_info->x_max = head->info_num;\
	init_x_final(num); \
}




/*
 * Sort features in priority_table by priority.
 */
static bool ang_sort_comp_priority(vptr UNUSED u, vptr UNUSED v, int a, int b)
{
	return (priority_table[a]->priority >= priority_table[b]->priority);
}

/*
 * Swap hook for features in priority_table.
 */
static void ang_sort_swap_priority(vptr UNUSED u, vptr UNUSED v, int a, int b)
{
	feature_type *f_ptr = priority_table[a];
	priority_table[a] = priority_table[b];
	priority_table[b] = f_ptr;
}

/*
 * Initialise the priority table for the small scale dungeon map.
 * This could be stored in a raw file if desired.
 */
static void init_feature_priorities(void)
{
	feature_type *f_ptr;
	int t;
	for (f_ptr = f_info, t = 0; f_ptr < f_info+z_info->f_max; f_ptr++)
	{
		if (f_info+f_ptr->mimic == f_ptr) t++;
	}

	feature_priorities = t;
	priority_table = C_NEW(t, feature_type *);

	for (f_ptr = f_info, t = 0; f_ptr < f_info+z_info->f_max; f_ptr++)
	{
		if (f_info+f_ptr->mimic == f_ptr)
		{
			priority_table[t++] = f_ptr;
		}
	}

	/* Sort by decreasing priority, so that only the first match matters. */
	ang_sort(0, 0, t, ang_sort_comp_priority, ang_sort_swap_priority);
}

/*** Initialize others ***/



/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i, j, n;
	option_type *op_ptr;

	/*** Prepare the "dungeon" information ***/

	/* Allocate and Wipe the object list */
	C_MAKE(o_list, MAX_O_IDX+INVEN_TOTAL, object_type);

	/* Divide the object list into player and dungeon sections. */
	inventory = o_list + MAX_O_IDX;

	/* Allocate and Wipe the monster list */
	C_MAKE(m_list, MAX_M_IDX, monster_type);

	/* Hack - the player can always see herself. */
	m_list[0].ml = TRUE;

	/* Allocate and wipe each line of the cave */
	for (i = 0; i < MAX_HGT; i++)
	{
		/* Allocate one row of the cave */
		C_MAKE(cave[i], MAX_WID, cave_type);
	}


	/*** Prepare the various "bizarre" arrays ***/

	/* Macro variables */
	C_MAKE(macro__pat, MACRO_MAX, cptr);
	C_MAKE(macro__act, MACRO_MAX, cptr);

	/* Macro action buffer */
	C_MAKE(macro__buf, 1024, char);

	/* Quark variables */
	C_MAKE(quark__str, QUARK_MAX, cptr);

	/* Set quark 0 to something safe. */
	quark__str[0] = "";

	/* Message variables */
	C_MAKE(message__ptr, MESSAGE_MAX, u16b);
	C_MAKE(message__buf, MESSAGE_BUF, char);


	/*** Prepare the Stores ***/

	/* Allocate the stores */
	C_MAKE(store, MAX_STORES_TOTAL, store_type);

	/* Fill in each store */
	for (i = 0; i < MAX_TOWNS; i++)
	{
		for (j = 0; j < MAX_STORES_PER_TOWN; j++)
		{
			/* Access the store */
			store_type *st_ptr = &store[i * MAX_STORES_PER_TOWN + j];

			/* Assume full stock */
			st_ptr->stock_size = STORE_INVEN_MAX;

			/* Allocate the stock */
			C_MAKE(st_ptr->stock, st_ptr->stock_size, object_type);

			/* Get the type of store */
			st_ptr->type = town_defs[i].store[j];
		}
	}

	/*** Prepare the options ***/

	/* Scan the options */
	for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
	{
		/* Set the variable to its default value. */
		op_ptr->o_var[0] = op_ptr->o_norm;
	}

	/* Analyze the windows */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < NUM_DISPLAY_FUNCS; i++)
		{
			/* Accept */
			windows[n].mask |= display_func[i].flag;
		}
	}


	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("");

	/* Prepare the stat_default array */
	{
		s16b stat[A_MAX];
		C_MAKE(stat_default, MAX_STAT_DEFAULT, stat_default_type);
		for (i = 0; i < A_MAX; i++) stat[i] = 8;
		add_stats(0, 0, 0, DEFAULT_STATS, stat, "Default");
	}

	/* Initialise the term_wins array. */
	init_term_wins();

	/* Initialise the chaos feature information. */
	init_chaos();

	/* Initialise the feature priority table. */
	init_feature_priorities();

#ifdef ALLOW_VISUALS
	/* Copy across the sizes of the visual tables. */
	init_visuals();
#endif /* ALLOW_VISUALS */

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
	for (i = 1; i < MAX_R_IDX; i++)
	{
		/* Get the i'th race */
		r_ptr = &r_info[i];

		/* Don't count "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

		/* Count the entries */
		alloc_race_size++;

		/* Group by level */
		num[r_ptr->level]++;
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

	/* Scan the monsters. */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		int p, x, y, z;

		/* Get the i'th race */
		r_ptr = &r_info[i];

		/* Don't count "fake" monsters. */
		if (is_fake_monster(r_ptr)) continue;

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

		/* Another entry complete for this locale */
		aux[x]++;
	}


	/* Success */
	return (0);
}



#define FOR_ALL_IN_CNT(ARRAY, PTR, CNT) \
	for ((PTR) = (ARRAY), (CNT) = 0; (PTR) < END_PTR(ARRAY); (PTR)++, (CNT)++)

#ifdef CHECK_ARRAYS

/*
 * Check screen_coords.
 */
static void check_screen_coords(void)
{
	const redraw_type *ptr;
	int i;
	FOR_ALL_IN_CNT(screen_coords, ptr, i)
	{
		if (ptr->idx != i)
		{
			quit_fmt("The %s screen co-ordinates have index %d rather than %d",
				ptr->name, i, ptr->idx);
		}
	}
}

/*
 * Check that skill_set is ordered correctly.
 */
static void check_skill_set(void)
{
	const player_skill *ptr;
	for (ptr = skill_set; ptr < END_PTR(skill_set); ptr++)
	{
		if (strlen(ptr->name) >= SKILL_NAME_LEN)
		{
			quit_fmt("SKILL_NAME_LEN is too short for \"%s\".", ptr->name);
		}
		else if (ptr != skill_set+ptr->idx)
		{
			quit_fmt("The %s skill has index %d rather than %d.", ptr->name,
				ptr - skill_set, ptr->idx);
		}
	}
}

/*
 * Check that option_info[] avoids putting too many options into a category,
 * or use the same bit in the save file to denote two options.
 */
static void check_options(void)
{
	bool flag_error = FALSE, error = FALSE;
	const option_type *op_ptr;
	int n[OPTS_MAX];
	u32b flag[8];

	WIPE(n, n);
	WIPE(flag, flag);

	for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
	{
		/* Negative categories are special. */
		if (n[op_ptr->o_page]++ >= MAX_OPTS_PER_PAGE)
		{
			plog_fmt("The %s option overflows its category.", op_ptr->o_text);
			error = TRUE;
		}
		if (flag[op_ptr->o_set] & (1L << op_ptr->o_bit))
		{
			plog_fmt(
				"The %s option shares a set and a bit with another option.",
				op_ptr->o_text);
			flag_error = error = TRUE;
		}
		flag[op_ptr->o_set] |= (1L << op_ptr->o_bit);
	}
	if (flag_error)
	{
		/* Draw a "helpful" table. */
		int i,j;
		char buf[40], *s;

		/* Introduction. */
		plog("The following table shows the unused bits as dots.");

		/* Draw the table. */
		for (i = 0; i < 8; i++)
		{
			for (j = 0, s = buf; j < 32; j++)
			{
				if (flag[i] & (1L << j))
				{
					*s++ = '*';
				}
				else
				{
					*s++ = '.';
				}
				if ((j % 4) == 3) *s++ = ' ';
			}
			*s++ = '\0';
			plog(buf);
		}
	}
	if (error)
	{
		/* Exit. */
		quit("Failed to parse option_info.");
	}
}

/*
 * Check various things about ma_blows[].
 */
static void check_ma_blows(void)
{
	bool first_attack = FALSE;
	martial_arts *ma_ptr;

	/* ma_blows[MAX_MA] is used for unskilled attacks. */
	assert(!ma_blows[MAX_MA].min_level);
	assert(!ma_blows[MAX_MA].chance);

	for (ma_ptr = ma_blows; ma_ptr < ma_blows+MAX_MA; ma_ptr++)
	{
		/* Check that a player who passes the "min_level" check for the easiest
		 * attack has a "chance" of using at least one attack. */
		if (ma_ptr->min_level == ma_blows->min_level &&
			ma_ptr->chance <= ma_blows->min_level)
		{
			first_attack = TRUE;
		}

		if (ma_ptr != ma_blows && ma_ptr->min_level < ma_ptr[-1].min_level)
		{
			quit_fmt("Mis-ordered martial arts techniques: %d < %d.",
				ma_ptr->min_level, ma_ptr[-1].min_level);
		}

		/* Field silliness. */
		if (ma_ptr->min_level < 0 || ma_ptr->min_level > 100 ||
			ma_ptr->chance < -1 || ma_ptr->chance > 99 ||
			ma_ptr->dd < 0 || ma_ptr->ds < 0)
		{
			quit_fmt("Martial arts technique \"%s\" (%d) malformed.",
				ma_ptr->desc, ma_ptr-ma_blows);
		}
	}

	if (!first_attack)
		quit_fmt("There is a %d%% minimum attack, but no attack can be used "
		"at %d%% skill.", ma_blows->min_level, ma_blows->min_level);
}

static void check_feeling_str(void)
{
	cptr_ch *ptr;
	FOR_ALL_IN(feeling_str, ptr)
	{
		if (ptr->idx != ptr - feeling_str)
			quit_fmt("feeling_str[] incorrectly ordered.");
	}
}


/*
 * Check that the members of various arrays are in the correct order,
 * by calling functions which quit if this is not the case.
 * This should be called when any of the arrays listed below may have changed
 * as the rest of code may assume that this is correct.
 */
static void check_arrays(void)
{
	check_bonus_table();
	check_screen_coords();
	check_temp_effects();
	check_options();
	check_skill_set();
	check_activation_info();
	check_magic_info();
	check_ma_blows();
	check_feeling_str();

	format("%v", equippy_f0);
}
#else /* CHECK_ARRAYS */
/*
 * Do nothing.
 */
static void check_arrays(void)
{
}
#endif /* CHECK_ARRAYS */

/*
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
	mc_put_fmt(23, 0, "%20s%s%v", "", str, clear_f0);
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

	char buf[1024];
	header head[1];

	/* Hack - a pointer intended not to match anything. */
	cptr dummy = buf;

	/* Paranoia - check the version. */
	if (strlen(GAME_VERSION) >= MAX_VERSION_LEN)
		quit("Version string too long.");

	WIPE(head, header);

	/* Hack - never call this twice. */
	if (z_info) return;


	/*** Verify the "news" file ***/

	/* Build the filename */
	strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_FILE, "news.txt");

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
	(void)fd_close(fd);


	/*** Display the "news" file ***/

	/* Clear screen */
	Term_clear();

	/* Display the News file */
	showfile("news.txt", 1);


	/*** Verify (or create) the "high score" file ***/

	/* Build the filename */
	strnfmt(buf, 1024, "%v", path_build_f2, ANGBAND_DIR_APEX, "scores.raw");

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
	(void)fd_close(fd);


	/* Hack - find the POSIX check_modification_date() function here. */
#if defined(CHECK_MODIFICATION_TIME) && defined(HAS_STAT)
	check_modification_date_hook = check_modification_date;
#endif /* CHECK_MODIFICATION_TIME && HAS_STAT */

	/*** Initialize some arrays ***/

	note("[Initializing array maxima...]");

	init_x_info("maxima", maxima, parse_z_info, "z_info", z_info,
		dummy, dummy, u_max, Z_HEAD);

#ifdef ALLOW_TEMPLATES
	/* Initialise the fake arrays if needed. */
	C_MAKE(head->fake_info_ptr, z_info->fake_info_size, char);
	C_MAKE(head->fake_name_ptr, z_info->fake_name_size, char);
	C_MAKE(head->fake_text_ptr, z_info->fake_text_size, char);

	/* initialisation macros are only used in init1.c. */
	init_x_info("macros", init_macro_type, parse_macro_info, "macro",
		macro_info, macro_name, macro_text, macros, MACRO_HEAD)
#endif /* ALLOW_TEMPLATES */

	init_x_info("features", feature_type, parse_f_info, "f_info", f_info,
		f_name, dummy, f_max, F_HEAD)

	init_x_info("objects", object_kind, parse_k_info, "k_info", k_info,
		k_name, k_text, k_max, K_HEAD)

	init_x_info("base objects", o_base_type, parse_o_base, "o_base", o_base,
		ob_name, dummy, ob_max, OB_HEAD)

	/* Initialize unidentified object info
	 * This leaves space for scrolls, and checks that it is large
	 * eough to map k_info, so must occur after init_k_info().
	 */
	init_x_info("unidentified object", unident_type, parse_u_info, "u_info",
		u_info, u_name, dummy, u_max, U_HEAD)

	/* Initialize artifact info */
	init_x_info("artifacts", artifact_type, parse_a_info, "a_info", a_info,
		a_name, dummy, a_max, A_HEAD)

	/* Initialize ego-item info */
	init_x_info("ego-items", ego_item_type, parse_e_info, "e_info", e_info,
		e_name, dummy, e_max, E_HEAD)

	/* Initialize monster info */
	init_x_info("monsters", monster_race, parse_r_info, "r_info", r_info,
		r_name, r_text, r_max, R_HEAD)

	/* Initialize death events. *
	 * Must come after init_(k|a|e|r)_info(). */
	init_x_info("death events", monster_race, parse_r_event, "r_event",
	death_event, event_name, event_text, event_max, EVENT_HEAD)

	/* Initialize dungeons. */
	init_x_info("dungeons", dun_type, parse_dun_defs, "d_dun", dun_defs,
		dun_name, dummy, dungeons, D_HEAD)

	/* Initialize towns. */
	init_x_info("towns", town_type, parse_town_defs, "d_town", town_defs,
		town_name, dummy, towns, T_HEAD)

	/* Initialize quests.
	 * Must come after parse_r_info() and parse_dun_defs().
	 * This array is only actually used when a new game is started.
	 */
	init_x_info("quests", quest_type, parse_q_list, "d_quest", q_list,
		dummy, dummy, quests, Q_HEAD)

	/* Initialise vault info */
	init_x_info("vaults", vault_type, parse_v_info, "v_info", v_info,
		v_name, v_text, v_max, V_HEAD)

	/* Initialise shopkeeper info */
	init_x_info("shopkeepers", owner_type, parse_s_info, "s_info", owners,
		s_name, dummy, owners, S_HEAD)

	/* Initialise player template info. */
	init_x_info("player templates", player_template, parse_template, "template",
		template_info, tp_name, dummy, templates, TPL_HEAD)

	/* Delete the fake arrays, we're done with them. */
	KILL(head->fake_info_ptr);
	KILL(head->fake_name_ptr);
	KILL(head->fake_text_ptr);

#ifdef ALLOW_TEMPLATES
	/* Delete the initialisation macro arrays, we're done with them. */
	KILL(macro_info);
	KILL(macro_text);
	KILL(macro_name);
#endif /* ALLOW_TEMPLATES */

	/* Initialize some other arrays */
	note("[Initializing arrays... (other)]");
	init_other();

	/* Initialize some other arrays */
	note("[Initializing arrays... (alloc)]");
	init_alloc();


	/*** Load default user pref files ***/

	/* Initialize feature info */
	note("[Initializing user pref files...]");

	/* Initialise the help file links. */
	init_help_files();

	/* Initialise ascii_to_text(). */
	init_ascii_text_conv();

	/* Access the "basic" pref file */
	strcpy(buf, "pref.prf");

	/* Process that file */
	process_pref_file(buf);

	/* Access the "user" pref file */
	sprintf(buf, "user.prf");

	/* Process that file */
	process_pref_file(buf);

	/* Access the "basic" system pref file */
	sprintf(buf, "pref-%s.prf", ANGBAND_SYS);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "user" system pref file */
	sprintf(buf, "user-%s.prf", ANGBAND_SYS);

	/* Process that file */
	process_pref_file(buf);

	/* Flush any messages the pref files created. */
	msg_print(NULL);

	/* Check that various arrays are in the correct order if required. */
	check_arrays();

	/* Done */
	note("[Initialization complete]");
}

/*
 * Free resources used by the game.
 */
void cleanup_angband(void)
{
}
