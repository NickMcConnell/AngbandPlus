#define INIT2_C
/* File: init2.c */

/* Purpose: Initialization (part 2) -BEN- */

#include "angband.h"

#ifdef CHECK_MODIFICATION_TIME
#include <sys/types.h>
#include <sys/stat.h>
#endif /* CHECK_MODIFICATION_TIME */

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
void init_file_paths(cptr path)
{
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
	ANGBAND_DIR_SAVE = string_make(format("%s%s", path, "save"));
	ANGBAND_DIR_USER = string_make(format("%s%s", path, "user"));
	ANGBAND_DIR_XTRA = string_make(format("%s%s", path, "xtra"));

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
	"too few arguments",
	"too many arguments",
	"too many allocation entries",
	"invalid spell frequency",
	"incorrect syntax",
};


#endif

#ifdef CHECK_MODIFICATION_TIME

static errr check_modification_date(int fd, cptr template_file)
{
	char buf[1024];

	struct stat txt_stat, raw_stat;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, template_file);

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

#endif /* CHECK_MODIFICATION_TIME */


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
static void init_info(header *head)
{
	cptr filename = head->file_name;

	int fd;

	errr err = 1;

	FILE *fp;

	/* General buffer */
	char buf[1024];


#ifdef ALLOW_TEMPLATES

	/*** Load the binary image file ***/

	if (~rebuild_raw & 1<<(head->header_num))
	{
		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_DATA, format("%s.raw", filename));

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

	/* Build the filename */

	/* Hack - the death_event array is read from r_info.txt, not
	 * r_event.txt. */
	if (head->header_num == EVENT_HEAD)
	{
		path_build(buf, 1024, ANGBAND_DIR_EDIT, "r_info.txt");
	}
	else
	{
		path_build(buf, 1024, ANGBAND_DIR_EDIT, format("%s.txt", filename));
	}

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp) quit(format("Cannot open '%s.txt' file.", filename));

	/* Parse the file */
	err = init_info_txt(fp, buf, head);

	/* Close it */
	my_fclose(fp);

	/* Errors */
	if (err) display_parse_error(filename, err, buf);


	/*** Dump the binary image file ***/

	/* File type is "DATA" */
	FILE_TYPE(FILE_TYPE_DATA);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_DATA, format("%s.raw", filename));


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
	path_build(buf, 1024, ANGBAND_DIR_DATA, format("%s.raw", filename));

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

static errr init_u_info_final(void)
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
	return SUCCESS;
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
	}
	return;
}
		

#define init_x_info(title, type, parse, file, x_info, x_name, x_text, x_max, num) \
{ \
	note(format("[Initializing arrays... (%s)]", title)); \
	init_header(type, num, parse, file); \
	init_info(head); \
	x_info = head->info_ptr; \
	if (x_name != dummy) x_name = head->name_ptr; \
	if (x_text != dummy) x_text = head->text_ptr; \
	z_info->x_max = head->info_num;\
	init_x_final(num); \
}






/*** Initialize others ***/



/*
 * Hack -- Objects sold in the stores -- by tval/sval pair.
 */
static byte store_table[MAX_STORE_TYPES][STORE_CHOICES][2] =
{
	{
		/* 0 = General Store */
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
		{ TV_CLOAK, SV_CLOAK },

		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },

		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_TORCH },
		{ TV_LITE, SV_LITE_LANTERN },
		{ TV_LITE, SV_LITE_LANTERN },

		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },
		{ TV_FLASK, 0 },

		{ TV_SHOT, SV_AMMO_NORMAL },
		{ TV_ARROW, SV_AMMO_NORMAL },
		{ TV_BOLT, SV_AMMO_NORMAL },
		{ TV_DIGGING, SV_SHOVEL }
	},

	{
		/* 1 = Armoury */

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
		{ TV_SHIELD, SV_SMALL_METAL_SHIELD },

		{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
		{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
		{ TV_HELM, SV_HARD_LEATHER_CAP },
		{ TV_HELM, SV_HARD_LEATHER_CAP },

		{ TV_SOFT_ARMOR, SV_ROBE },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
		{ TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },

		{ TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
		{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL },

		{ TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
		{ TV_GLOVES, SV_SET_OF_GAUNTLETS },
		{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD }
	},

	{
		/* 2 = Weaponsmith */
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

		{ TV_BOW, SV_LONG_BOW },
		{ TV_BOW, SV_LIGHT_XBOW },
		{ TV_ARROW, SV_AMMO_NORMAL },
		{ TV_ARROW, SV_AMMO_NORMAL },

		{ TV_BOLT, SV_AMMO_NORMAL },
		{ TV_BOLT, SV_AMMO_NORMAL },
		{ TV_BOW, SV_SHORT_BOW },
		{ TV_SWORD, SV_DAGGER },

		{ TV_SWORD, SV_MAIN_GAUCHE },
		{ TV_SWORD, SV_RAPIER },
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_SWORD, SV_SHORT_SWORD },

		{ TV_HAFTED, SV_WHIP },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SWORD, SV_LONG_SWORD },
		{ TV_SWORD, SV_SCIMITAR }
	},

	{
		/* 3 = Temple */
		{ TV_HAFTED, SV_WHIP },
		{ TV_HAFTED, SV_QUARTERSTAFF },
		{ TV_HAFTED, SV_MACE },
		{ TV_HAFTED, SV_BALL_AND_CHAIN },

		{ TV_HAFTED, SV_WAR_HAMMER },
		{ TV_HAFTED, SV_LUCERN_HAMMER },
		{ TV_HAFTED, SV_MORNING_STAR },
		{ TV_HAFTED, SV_FLAIL },

		{ TV_HAFTED, SV_LEAD_FILLED_MACE },
		{ TV_SCROLL, SV_SCROLL_REMOVE_CURSE },
		{ TV_SCROLL, SV_SCROLL_BLESSING },
		{ TV_SCROLL, SV_SCROLL_HOLY_CHANT },

		{ TV_POTION, SV_POTION_HEROISM },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },

		{ TV_POTION, SV_POTION_CURE_LIGHT },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },

		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },

		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },

		{ TV_POTION, SV_POTION_CURE_LIGHT },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_SERIOUS },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },

		{ TV_HAFTED, SV_WHIP },
		{ TV_HAFTED, SV_MACE },
		{ TV_HAFTED, SV_BALL_AND_CHAIN },
		{ TV_HAFTED, SV_WAR_HAMMER },

		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_POTION, SV_POTION_CURE_CRITICAL },

		{ TV_POTION, SV_POTION_CURE_CRITICAL },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },
		{ TV_POTION, SV_POTION_RESTORE_EXP },

		{ TV_SCROLL, SV_SCROLL_REMOVE_CURSE },
		{ TV_SCROLL, SV_SCROLL_REMOVE_CURSE },
		{ TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE },
		{ TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE }
	},

	{
		/* 4 = Alchemy shop */
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
		{ TV_SCROLL, SV_SCROLL_TELEPORT },
		{ TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION },

		{ TV_SCROLL, SV_SCROLL_MAPPING },
		{ TV_SCROLL, SV_SCROLL_DETECT_GOLD },
		{ TV_SCROLL, SV_SCROLL_DETECT_ITEM },
		{ TV_SCROLL, SV_SCROLL_DETECT_TRAP },

		{ TV_SCROLL, SV_SCROLL_DETECT_INVIS },
		{ TV_SCROLL, SV_SCROLL_RECHARGING },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },

		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
		{ TV_SCROLL, SV_SCROLL_TELEPORT },

		{ TV_SCROLL, SV_SCROLL_TELEPORT },
		{ TV_POTION, SV_POTION_RES_STR },
		{ TV_POTION, SV_POTION_RES_INT },
		{ TV_POTION, SV_POTION_RES_WIS },

		{ TV_POTION, SV_POTION_RES_DEX },
		{ TV_POTION, SV_POTION_RES_CON },
		{ TV_POTION, SV_POTION_RES_CHR },
		{ TV_SCROLL, SV_SCROLL_IDENTIFY },

		{ TV_SCROLL, SV_SCROLL_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_STAR_IDENTIFY },  /* Yep, occasionally! */
		{ TV_SCROLL, SV_SCROLL_STAR_IDENTIFY },
		{ TV_SCROLL, SV_SCROLL_LIGHT },

		{ TV_POTION, SV_POTION_RES_STR },
		{ TV_POTION, SV_POTION_RES_INT },
		{ TV_POTION, SV_POTION_RES_WIS },
		{ TV_POTION, SV_POTION_RES_DEX },

		{ TV_POTION, SV_POTION_RES_CON },
		{ TV_POTION, SV_POTION_RES_CHR },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_ARMOR },
		{ TV_SCROLL, SV_SCROLL_ENCHANT_ARMOR },

		{ TV_SCROLL, SV_SCROLL_RECHARGING },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER }

	},

	{
		/* 5 = Magic-User store */
		{ TV_RING, SV_RING_PROTECTION },
		{ TV_RING, SV_RING_FEATHER_FALL },
		{ TV_RING, SV_RING_PROTECTION },
		{ TV_RING, SV_RING_RESIST_FIRE },

		{ TV_RING, SV_RING_RESIST_COLD },
		{ TV_AMULET, SV_AMULET_CHARISMA },
		{ TV_AMULET, SV_AMULET_SLOW_DIGEST },
		{ TV_AMULET, SV_AMULET_RESIST_ACID },

		{ TV_AMULET, SV_AMULET_SEARCHING },
		{ TV_WAND, SV_WAND_SLOW_MONSTER },
		{ TV_WAND, SV_WAND_CONFUSE_MONSTER },
		{ TV_WAND, SV_WAND_SLEEP_MONSTER },

		{ TV_WAND, SV_WAND_MAGIC_MISSILE },
		{ TV_WAND, SV_WAND_STINKING_CLOUD },
		{ TV_WAND, SV_WAND_WONDER },
		{ TV_WAND, SV_WAND_DISARMING },

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
		{ TV_STAFF, SV_STAFF_TELEPORTATION },
		{ TV_STAFF, SV_STAFF_TELEPORTATION },

		{ TV_STAFF, SV_STAFF_IDENTIFY },
		{ TV_STAFF, SV_STAFF_IDENTIFY },
		{ TV_STAFF, SV_STAFF_IDENTIFY },
		{ TV_STAFF, SV_STAFF_IDENTIFY },

		{ TV_STAFF, SV_STAFF_IDENTIFY },
		{ TV_STAFF, SV_STAFF_REMOVE_CURSE },
		{ TV_STAFF, SV_STAFF_CURE_LIGHT },
		{ TV_STAFF, SV_STAFF_PROBING },

		{ TV_CHARM, 0 },
		{ TV_CHARM, 0 },
		{ TV_CHARM, 1 },
		{ TV_CHARM, 2 },

		{ TV_CHARM, 3 },
		{ TV_CHARM, 4 },
		{ TV_CHARM, 5 },
		{ TV_CHARM, 6 },

		{ TV_SORCERY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 0 },

	},

	{
		/* 6 = Black Market (unused) */
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
		/* 7 = Home (unused) */
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
		/* 8 = Bookstore */
		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 1 },
		{ TV_SORCERY_BOOK, 1 },

		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 1 },
		{ TV_SORCERY_BOOK, 1 },

		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 0 },
		{ TV_SORCERY_BOOK, 1 },
		{ TV_SORCERY_BOOK, 1 },

		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 1 },
		{ TV_THAUMATURGY_BOOK, 1 },

		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 1 },
		{ TV_THAUMATURGY_BOOK, 1 },

		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 0 },
		{ TV_THAUMATURGY_BOOK, 1 },
		{ TV_THAUMATURGY_BOOK, 1 },

		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 1 },
		{ TV_NECROMANCY_BOOK, 1 },

		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 1 },
		{ TV_NECROMANCY_BOOK, 1 },

		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 0 },
		{ TV_NECROMANCY_BOOK, 1 },
		{ TV_NECROMANCY_BOOK, 1 },

		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 1 },
		{ TV_CONJURATION_BOOK, 1 },

		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 1 },
		{ TV_CONJURATION_BOOK, 1 },

		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 0 },
		{ TV_CONJURATION_BOOK, 1 },
		{ TV_CONJURATION_BOOK, 1 },
	},

	{
		/* 9 = Inn  - A bit repetitive, this one... */
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
		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },

		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },
		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },

		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },

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
		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },

		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },
		{ TV_FOOD, SV_FOOD_RATION },

		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },
		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },

		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
		{ TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },

		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },
		{ TV_FOOD, SV_FOOD_PINT_OF_WINE },
		{ TV_FOOD, SV_FOOD_PINT_OF_ALE },
	},

	{
		/* 10 = Hall (unused) */
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
		/* 11 = Pawnbrokers (unused) */
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
	}
};




/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
	int i, j, k, n;

	/*** Prepare the "dungeon" information ***/

	/* Allocate and Wipe the object list */
	C_MAKE(o_list, MAX_O_IDX, object_type);

	/* Allocate and Wipe the monster list */
	C_MAKE(m_list, MAX_M_IDX, monster_type);


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
	C_MAKE(macro__cmd, MACRO_MAX, bool);

	/* Macro action buffer */
	C_MAKE(macro__buf, 1024, char);

	/* Quark variables */
	C_MAKE(quark__str, QUARK_MAX, cptr);

	/* Set quark 0 to something safe. */
	quark__str[0] = "";

	/* Message variables */
	C_MAKE(message__ptr, MESSAGE_MAX, u16b);
	C_MAKE(message__buf, MESSAGE_BUF, char);

	/* Hack -- No messages yet */
	message__tail = MESSAGE_BUF;


	/*** Prepare the Player inventory ***/

	/* Allocate it */
	C_MAKE(inventory, INVEN_TOTAL, object_type);


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

			/* No table for the black market or home */
			if ((st_ptr->type == STORE_BLACK) ||
				(st_ptr->type == STORE_HOME) ||
				(st_ptr->type == 99)) continue;

			/* Assume full table */
			st_ptr->table_size = STORE_CHOICES;

			/* Allocate the stock */
			C_MAKE(st_ptr->table, st_ptr->table_size, s16b);

			/* Scan the choices */
			for (k = 0; k < STORE_CHOICES; k++)
			{
				int k_idx;

				/* Extract the tval/sval codes */
				int tv = store_table[st_ptr->type][k][0];
				int sv = store_table[st_ptr->type][k][1];

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
	}

	/*** Prepare the options ***/

	/* Scan the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_set;
		int ob = option_info[i].o_bit;

		/* Set the "default" options */
		if (option_info[i].o_var)
		{
			/* Accept */
			option_mask[os] |= (1L << ob);
			
			/* Set */
			if (option_info[i].o_norm)
			{
				/* Set */
				option_flag[os] |= (1L << ob);
			}
			
			/* Clear */
			else
			{
				/* Clear */
				option_flag[os] &= ~(1L << ob);
			}
		}
	}

	/* Analyze the windows */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Accept */
			if (window_flag_desc[i])
			{
				/* Accept */
				windows[n].mask |= (1L << i);
			}
		}
	}


	/*** Pre-allocate space for the "format()" buffer ***/

	/* Hack -- Just call the "format()" function */
	(void)format("%s (%s).", "Dean Anderson", MAINTAINER);

	/* Prepare the stat_default array */
	C_MAKE(stat_default, MAX_STAT_DEFAULT, stat_default_type);
	(void)add_stats(0,0,0,DEFAULT_STATS,8,8,8,8,8,8,"Default");

	/* Initialise the term_wins array. */
	init_term_wins();

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
	header head[1];

	/* Hack - a pointer intended not to match anything. */
	vptr dummy = (vptr)&init_angband;

	/* Hack - never call this twice. */
	if (z_info) return;


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
	(void)fd_close(fd);


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
	(void)fd_close(fd);


	/*** Initialize some arrays ***/

	note("[Initializing array maxima...]");

	init_x_info("maxima", maxima, parse_z_info, "z_info", z_info,
		dummy, dummy, u_max, Z_HEAD);

	/* Initialise the fake arrays now their sizes are known. */
	C_MAKE(head->fake_info_ptr, z_info->fake_info_size, char);
	C_MAKE(head->fake_name_ptr, z_info->fake_name_size, char);
	C_MAKE(head->fake_text_ptr, z_info->fake_text_size, char);

	/* initialisation macros are only used in init1.c. */
#ifdef ALLOW_TEMPLATES
	init_x_info("macros", init_macro_type, parse_macro_info, "macro",
		macro_info, macro_name, macro_text, macros, MACRO_HEAD)
#endif /* ALLOW_TEMPLATES */

	init_x_info("features", feature_type, parse_f_info, "f_info", f_info,
		f_name, f_text, f_max, F_HEAD)

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
	note("[Initializing arrays... (monsters)]");
	init_x_info("monsters", monster_race, parse_r_info, "r_info", r_info,
		r_name, r_text, r_max, R_HEAD)

	/* Initialize death events. *
	 * Must come after init_(k|a|e|r)_info(). */
	note("[Initializing arrays... (death events)]");
	init_x_info("death events", monster_race, parse_r_event, "r_event",
	death_event, event_name, event_text, event_max, EVENT_HEAD)
 
	/* Initialize feature info */
	note("[Initializing arrays... (vaults)]");
	init_x_info("vaults", vault_type, parse_v_info, "v_info", v_info,
		v_name, v_text, v_max, V_HEAD)

	/* Delete the fake arrays, we're done with them. */
	KILL(head->fake_info_ptr);
	KILL(head->fake_name_ptr);
	KILL(head->fake_text_ptr);

	/* Delete the initialisation macro arrays, we're done with them. */
	KILL(macro_info);
	KILL(macro_text);
	KILL(macro_name);

	/* Initialize some other arrays */
	note("[Initializing arrays... (other)]");
	init_other();

	/* Initialize some other arrays */
	note("[Initializing arrays... (alloc)]");
	init_alloc();


	/*** Load default user pref files ***/

	/* Initialize feature info */
	note("[Initializing user pref files...]");

	/* build a name for the basic 'help' index */
	syshelpfile = "help.hlp";

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

	/* Done */
	note("[Initialization complete]");
}

/*
 * Free resources used by the game.
 */
void cleanup_angband(void)
{
}
