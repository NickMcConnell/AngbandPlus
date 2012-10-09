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
 * In general, the initial path should end in the appropriate
 * "PATH_SEP" string.  All of the "sub-directory" paths created
 * below will also end in the "PATH_SEP" string, and, in general,
 * any replacement paths specified by the user should do the same.
 *
 * Mega-Hack -- support fat raw files under NEXTSTEP, using
 * a special "system prefix" for all the "lib/data" files.
 * This will cause the "lib/data" directory to hold one set
 * of "raw" files for each relevant platform, when necessary,
 * and still hold the "normal" files if no platform is known.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet.
 */
void init_file_paths(char *path)
{
   char *tail;

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
   (void)string_free(ANGBAND_DIR_USER);
   (void)string_free(ANGBAND_DIR_XTRA);
   (void)string_free(ANGBAND_DIR_TEMP);

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
   ANGBAND_DIR_TEMP = string_make("");

#else /* VM */

   /*** Build the sub-directory names ***/

   /* Build a path name */
   sprintf(tail, "apex%s", PATH_SEP);
   ANGBAND_DIR_APEX = string_make(path);

   /* Build a path name */
   sprintf(tail, "bone%s", PATH_SEP);
   ANGBAND_DIR_BONE = string_make(path);

   /* Build a path name */
   sprintf(tail, "data%s", PATH_SEP);
   ANGBAND_DIR_DATA = string_make(path);

   /* Build a path name */
   sprintf(tail, "edit%s", PATH_SEP);
   ANGBAND_DIR_EDIT = string_make(path);

   /* Build a path name */
   sprintf(tail, "file%s", PATH_SEP);
   ANGBAND_DIR_FILE = string_make(path);

   /* Build a path name */
   sprintf(tail, "help%s", PATH_SEP);
   ANGBAND_DIR_HELP = string_make(path);

   /* Build a path name */
   sprintf(tail, "info%s", PATH_SEP);
   ANGBAND_DIR_INFO = string_make(path);

   /* Build a path name */
   sprintf(tail, "save%s", PATH_SEP);
   ANGBAND_DIR_SAVE = string_make(path);

   /* Build a path name */
   sprintf(tail, "user%s", PATH_SEP);
   ANGBAND_DIR_USER = string_make(path);

   /* Build a path name */
   sprintf(tail, "xtra%s", PATH_SEP);
   ANGBAND_DIR_XTRA = string_make(path);

/* jk */
   /* Build a path name */
   sprintf(tail, "temp%s", PATH_SEP);
   ANGBAND_DIR_TEMP = string_make(path);

#endif /* VM */

#ifdef NeXT

   /* Allow "fat binary" usage with NeXT */
   if (TRUE)
   {
      cptr next = "";

# if defined(m68k)
      next = "m68k.";
# endif

# if defined(i386)
      next = "i386.";
# endif

# if defined(sparc)
      next = "sparc.";
# endif

# if defined(hppa)
      next = "hppa.";
# endif

      /* Forget the old path name */
      string_free(ANGBAND_DIR_DATA);

      /* Build a new path name */
      sprintf(tail, "data%s%s", PATH_SEP, next);
      ANGBAND_DIR_DATA = string_make(path);
   }

#endif /* NeXT */

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
   plog("The 'lib' directory is probably missing or broken.\n");

   /* More details */
   plog("Perhaps the archive was not extracted correctly.\n");

   /* More details */
   plog("Or an environment variable pointing to this dir is not set.\n");

   /* Explain */
   plog("See the 'QuickStart' file for more information.\n");

   /* Quit with error */
   quit("Fatal Error.\n");
}

/*
 * Hack -- verify some files, and display the "news.txt" file
 *
 * This function is often called before "init_some_arrays()",
 * but after the "term.c" package has been initialized, so
 * be aware that many functions will not be "usable" yet.
 *
 * Note that this function attempts to verify the "news" file,
 * and the game aborts (cleanly) on failure.
 *
 * Note that this function attempts to verify (or create) the
 * "high score" file, and the game aborts (cleanly) on failure.
 *
 * Note that one of the most common "extraction" errors involves
 * failing to extract all sub-directories (even empty ones), such
 * as by failing to use the "-d" option of "pkunzip", or failing
 * to use the "save empty directories" option with "Compact Pro".
 * This error will often be caught by the "high score" creation
 * code below, since the "lib/apex" directory, being empty in the
 * standard distributions, is most likely to be "lost", making it
 * impossible to create the high score file.
 */
void init_angband(void)
{
   s16b      fd = -1;

   s16b      mode = 0644;

   FILE     *fp;

   char     buf[1024];


   /*** Verify the "news" file ***/

   /* Access the "news.txt" file */
   strcpy(buf, ANGBAND_DIR_FILE);
   strcat(buf, "news.txt");

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

   /* Clear the screen */
   clear_screen();

   /* Access the "news" file */
   strcpy(buf, ANGBAND_DIR_FILE);
   strcat(buf, "news.txt");

   /* Open the News file */
   fp = my_fopen(buf, "r");

   /* Dump */
   if (fp)
   {
      s16b i = 0;

      /* Dump the file to the screen */
      while (0 == my_fgets(fp, buf, 1024))
      {
         /* Display and advance */
         put_str(buf, 0, i++);
      }

      /* Close */
      (void)my_fclose(fp);
   }

   (void)Term_fresh();

   /*** Verify (or create) the "high score" file ***/

   /* Access the high score file */
   strcpy(buf, ANGBAND_DIR_APEX);
   strcat(buf, "scores.raw");

   /* Attempt to open the high score file */
   fd = fd_open(buf, O_RDONLY );

   /* Failure */
   if (fd < 0)
   {

#if defined(MACINTOSH) && !defined(applec)
      /* Global -- "data file" */
      _ftype = 'DATA';
#endif

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
}

/*
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
   (void)Term_erase(0, 23, 80);
   (void)Term_putstr(20, 23, -1, TERM_WHITE, str);
   (void)Term_fresh();
}

/*
 * Hack -- help initialize the fake "name" and "text" arrays when
 * parsing an "ascii" template file.
 */
u16b fake_name_size;
u16b fake_text_size;

/*
 * Standard error message text
 */
#define MAX_ERR_STR 11
static cptr err_str[MAX_ERR_STR] =
{
   "unknown error",                         /*  0  */
   "parse error",                           /*  1  */
   "obsolete file",                         /*  2  */
   "missing record header",                 /*  3  */
   "non-sequential records",                /*  4  */
   "invalid flag specification",            /*  5  */
   "undefined directive",                   /*  6  */
   "out of memory",                         /*  7  */
   "too many records",                      /*  8  */
   "file unreadable",                       /*  9  */
   "< without > in template-file"           /* 10  */
};

/*** Initialize from binary image files ***/

/*
 * Initialize the "f_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_f_info(void)
{
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1, mtyp, i;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "f_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   (void)fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("f_info.hdr", &f_number, MAX_F_IDX,
                           &f_name_size, &f_text_size);

      /* Allocate the "f_info" array */
      C_MAKE(f_info, f_number, feature_type);
      C_MAKE(f_name, f_name_size, char);
      C_MAKE(f_text, f_text_size, char);

      err=read_info_data("f_info.raw", (char*)f_info, f_number, sizeof(feature_type),
                         f_name, f_name_size, f_text, f_text_size);
   }
   else
      (void)fd_close(fd);

dlog(DEBUGTEMPL,"init2.c: init_f_info: err now %d\n", err);
#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if ((err != 0) || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "f_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "f_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "rb");

dlog(DEBUGTEMPL,"init2.c: init_f_info: reading f_info from %s\n", buf);
      /* Parse it */
      if (!fp) quit("Cannot open 'f_info.txt' file.");

      /* Assume the size of "f_name" and "f_text" */
      f_name_size = 3 * 1024L;
      f_text_size = 1 * 1024L;

      /* Allocate the "f_info" array */
      C_MAKE(f_info, MAX_F_IDX, feature_type);
      C_MAKE(f_name, f_name_size, char);
      C_MAKE(f_text, f_text_size, char);

      /* Parse the file */
      err = init_f_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'f_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_F_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit(format("Error (type %d(%s) line %d of file %s.",
                  err, oops, error_line, buf));
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading f_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in f_info data file.");
   }
#endif
   /* jk */
   /* we build a cache of entries in f_info, with the first f_idx where */
   /* a certain mtyp occurs - this requires that they occur in order!   */
   for (mtyp=0; mtyp<MAX_F_IDX_MTYP; mtyp++)
   {
      for (i=0; i<f_number; i++)
      {
         if (f_info[i].mtyp==mtyp) break;
      }
      if (i==f_number)
      {
         quit(format("Error: mtyp %d not found in f_info array",mtyp));
      }
      f_info_index[mtyp]=i;
   }

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
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "k_info.hdr");
   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("k_info.hdr", &k_number, MAX_K_IDX,
                           &k_name_size, &k_text_size);
      /* Allocate the "k_info" array */
      C_MAKE(k_info, k_number, object_kind);
      C_MAKE(k_name, k_name_size, char);
      C_MAKE(k_text, k_text_size, char);

      err=read_info_data("k_info.raw", (char*)k_info, k_number, sizeof(object_kind),
                         k_name, k_name_size, k_text, k_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "k_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "k_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 'k_info.txt' file.");

      /* Assume the size of "k_name" and "k_text" */
      k_name_size = 9 * 1024L;
      k_text_size = 48 * 1024L;

      /* Allocate the "k_info" array */
      C_MAKE(k_info, MAX_K_IDX, object_kind);
      C_MAKE(k_name, k_name_size, char);
      C_MAKE(k_text, k_text_size, char);

      /* Parse the file */
      err = init_k_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'k_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_K_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 'k_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading k_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in k_info data file.");
   }
#endif

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
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "a_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("a_info.hdr", &a_number, MAX_A_IDX,
                           &a_name_size, &a_text_size);

      /* Allocate the "a_info" array */
      C_MAKE(a_info, a_number, artifact_type);
      C_MAKE(a_name, a_name_size, char);
      C_MAKE(a_text, a_text_size, char);

      err=read_info_data("a_info.raw", (char*)a_info, a_number, sizeof(artifact_type),
                         a_name, a_name_size, a_text, a_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "a_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "a_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 'a_info.txt' file.");

      /* Assume the size of "a_name" and "a_text" */
      a_name_size = 3 * 1024L;
      a_text_size = 8 * 1024L;

      /* Allocate the "a_info" array */
      C_MAKE(a_info, MAX_A_IDX, artifact_type);
      C_MAKE(a_name, a_name_size, char);
      C_MAKE(a_text, a_text_size, char);

      /* Parse the file */
      err = init_a_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'a_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_A_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 'a_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading a_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in a_info data file.");
   }
#endif

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
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "e_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("e_info.hdr", &e_number, MAX_E_IDX,
                           &e_name_size, &e_text_size);

      /* Allocate the "e_info" array */
      C_MAKE(e_info, e_number, ego_item_type);
      C_MAKE(e_name, e_name_size, char);
      C_MAKE(e_text, e_text_size, char);

      err=read_info_data("e_info.raw", (char*)e_info, e_number, sizeof(ego_item_type),
                         e_name, e_name_size, e_text, e_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "e_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "e_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 'e_info.txt' file.");

      /* Assume the size of "e_name" and "e_text" */
      e_name_size = 3 * 1024L;
      e_text_size = 1 * 1024L;

      /* Allocate the "e_info" array */
      C_MAKE(e_info, MAX_E_IDX, ego_item_type);
      C_MAKE(e_name, e_name_size, char);
      C_MAKE(e_text, e_text_size, char);

      /* Parse the file */
      err = init_e_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'e_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_E_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 'e_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading e_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in e_info data file.");
   }
#endif

   return (0);
}

/*
 * jk - Initialize the "t_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_t_info(void)
{
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "t_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("t_info.hdr", &t_number, MAX_T_IDX,
                           &t_name_size, &t_text_size);

      /* Allocate the "t_info" array */
      C_MAKE(t_info, t_number, trap_type);
      C_MAKE(t_name, t_name_size, char);
      C_MAKE(t_text, t_text_size, char);

      err=read_info_data("t_info.raw", (char*)t_info, t_number, sizeof(trap_type),
                         t_name, t_name_size, t_text, t_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "t_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "t_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 't_info.txt' file.");

      /* Assume the size of "t_name" and "t_text" */
      t_name_size = 3 * 1024L;
      t_text_size = 5 * 1024L;

      /* Allocate the "t_info" array */
      C_MAKE(t_info, MAX_T_IDX, trap_type);
      C_MAKE(t_name, t_name_size, char);
      C_MAKE(t_text, t_text_size, char);

      /* Parse the file */
      err = init_t_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 't_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_T_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 't_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading t_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in t_info data file.");
   }
#endif

   return (0);
}

/*
 * jk - Initialize the "s_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_s_info(void)
{
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "s_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("s_info.hdr", &s_number, MAX_S_IDX,
                           &s_name_size, &s_text_size);
dlog(DEBUGALLOC,"init2.c: init_s_info: s_number %d s_name_size %d s_text_size %d\n",
                s_number, s_name_size, s_text_size);

      /* Allocate the "s_info" array */
      C_MAKE(s_info, s_number, spell_type);
      C_MAKE(s_name, s_name_size, char);
      C_MAKE(s_text, s_text_size, char);

      err=read_info_data("s_info.raw", (char*)s_info, s_number, sizeof(spell_type),
                         s_name, s_name_size, s_text, s_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "s_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "s_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 's_info.txt' file.");

      /* Assume the size of "s_name" and "s_text" */
      s_name_size = 3 * 1024L;
      s_text_size = 4 * 1024L;

      /* Allocate the "s_info" array */
      C_MAKE(s_info, MAX_S_IDX, spell_type);
      C_MAKE(s_name, s_name_size, char);
      C_MAKE(s_text, s_text_size, char);

      /* Parse the file */
      err = init_s_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 's_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_S_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 's_info.txt' file.");
   } /* no header file found - reading s_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading s_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in s_info data file.");
   }
#endif

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
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;
   u32b    k;

   /* General buffer */
   char    buf[1024];

dlog(DEBUGTEMPL,"init2.c: init_r_info: started force %d\n",
                force_templates);
   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "r_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      /* we read two headers, the first exluding ghosts   */
      /* and the second including ghosts. Ghost-slots are */
      /* empty in r_info.raw, we fill them later.         */
      /* see load.c, rd_ghost_files().                    */
      err=read_info_header("r_info.hdr", &r_number, MAX_R_IDX_NORMAL,
                           &r_name_size, &r_text_size);
      err=read_info_header("r_info_t.hdr", &r_number_total, MAX_R_IDX,
                           &r_name_size_total, &r_text_size_total);

      /* Allocate the "r_info" array */
      C_MAKE(r_info, r_number_total, monster_race);
      C_MAKE(r_name, r_name_size_total, char);
      C_MAKE(r_text, r_text_size_total, char);
dlog(DEBUGTEMPL,"init2.c: init_r_info: r_number %d r_name_size %d r_text_size %d\n",
                r_number, r_name_size, r_text_size);
      err=read_info_data("r_info.raw", (char*)r_info, r_number_total, sizeof(monster_race),
                         r_name, r_name_size_total, r_text, r_text_size_total);
   }
   else
   {
      (void)fd_close(fd);
   }

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "r_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "r_info.txt");

dlog(DEBUGTEMPL,"init2.c: init_r_info: about to read file %s\n", buf);
      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 'r_info.txt' file.");

      /* Assume the size of "r_name" and "r_text" */
      r_name_size = 14 * 1024L;
      r_name_size_total = r_name_size + MAX_GHOSTS * 32;
      r_text_size = 52 * 1024L;
      r_text_size_total = r_text_size + MAX_GHOSTS * 240;

      /* Allocate the "r_info" array */
      C_MAKE(r_info, MAX_R_IDX, monster_race);
dlog(DEBUGGHOST,"init2.c: init_r_info: allocating %d monster races\n", MAX_R_IDX);
      C_MAKE(r_name, r_name_size_total, char);
      C_MAKE(r_text, r_text_size_total, char);
dlog(DEBUGGHOST,"init2.c: init_r_info: allocating %ld name space, %ld text space\n",
                r_name_size_total, r_text_size_total);

      /* Parse the file */
      err = init_r_info_txt(fp, buf);

      if (!err)
      {
         /* wipe the ghosts' space */
         for (k=r_name_size; k<r_name_size_total; k++)
         {
            *(r_name + k) = 0;
         }
         for (k=r_text_size; k<r_text_size_total; k++)
         {
            *(r_text + k) = 0;
         }
      }

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'r_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_R_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 'r_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading r_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in r_info data file.");
   }
#endif
   return (0);
}

static errr init_v_info(void)
{
   errr    err = 0;
   FILE    *fp;
   s16b    fd = -1;

   /* General buffer */
   char    buf[1024];

   /* test for existance of binary data file */
   strcpy(buf, ANGBAND_DIR_DATA);
   strcat(buf, "v_info.hdr");

   /* open and close it again */
   fd = fd_open(buf, O_RDONLY);
   fd_close(fd);

   /* if opening succeeded and we don't want to rebuild these files, */
   /*  read the raw data from the .hdr and .raw files. */
   if ((fd>=0) && !force_templates)
   {
      err=read_info_header("v_info.hdr", &v_number, MAX_V_IDX,
                           &v_name_size, &v_text_size);

      /* Allocate the "v_info" array */
      C_MAKE(v_info, v_number, vault_type);
      C_MAKE(v_name, v_name_size, char);
      C_MAKE(v_text, v_text_size, char);

      err=read_info_data("v_info.raw", (char*)v_info, v_number, sizeof(vault_type),
                         v_name, v_name_size, v_text, v_text_size);
   }
   else
      (void)fd_close(fd);

#ifdef ALLOW_TEMPLATES
   /* do we have an error reading the data file, or we have none? */
   if (err || (fd<0) || force_templates)
   {
      /*** Load the ascii template file ***/

      /* Access the "v_info.txt" file */
      strcpy(buf, ANGBAND_DIR_EDIT);
      strcat(buf, "v_info.txt");

      /* Open the file */
      fp = my_fopen(buf, "r");

      /* Parse it */
      if (!fp) quit("Cannot open 'v_info.txt' file.");

      /* Assume the size of "v_name" and "v_text" */
      v_name_size = 6 * 1024L;
      v_text_size = 48 * 1024L;

      /* Allocate the "v_info" array */
      C_MAKE(v_info, MAX_V_IDX, vault_type);
      C_MAKE(v_name, v_name_size, char);
      C_MAKE(v_text, v_text_size, char);

      /* Parse the file */
      err = init_v_info_txt(fp, buf);

      /* Close it */
      my_fclose(fp);
   }
   /* Errors */
   if (err)
   {
      cptr oops;
      /* Error string */
      oops = (((err > 0) && (err < MAX_ERR_STR)) ? err_str[err] : "unknown");

      msg_format("Error %d at line %d of 'v_info.txt'.", err, error_line);
      msg_format("Record %d of %d contains a '%s' error.", error_idx+1, MAX_V_IDX, oops);
      msg_format("Parsing '%s'.", buf);
      msg_print(NULL);

      /* Quit */
      quit("Error in 'v_info.txt' file.");
   } /* no header file found - reading r_info.txt */
#else
   if (err)
   {
      msg_format("Error %d while reading v_info data file", err);
      msg_print(NULL);

      /* Quit */
      quit("Error in v_info data file.");
   }
#endif

   return (0);
}

/*** Initialize others ***/

/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
   int i, j;

   object_kind *k_ptr;

   monster_race *r_ptr;

   alloc_entry *table;

   s16b num[MAX_LEVEL];

   s16b aux[MAX_LEVEL];

   /*** Analyze object allocation info ***/
dlog(DEBUGALLOC,"init2.c: init_alloc starting\n");
   /* Clear the "aux" array */
   C_WIPE(&aux, MAX_LEVEL, s16b);

   /* Clear the "num" array */
   C_WIPE(&num, MAX_LEVEL, s16b);
dlog(DEBUGALLOC,"init2.c: init_alloc step1\n");

   /* Size of "alloc_kind_table" */
   alloc_kind_size = 0;

   /* Scan the objects */
   for (i = 1; i < k_number; i++)
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
dlog(DEBUGALLOC,"init2.c: init_alloc step2\n");

   /* Collect the level indexes */
   for (i = 1; i < MAX_LEVEL; i++)
   {
      /* Group by level */
      num[i] += num[i-1];
   }

   /* Paranoia */
   if (!num[0]) quit("No town objects!");
dlog(DEBUGALLOC,"init2.c: init_alloc step3\n");

   /*** Initialize object allocation info ***/

   /* Allocate the alloc_kind_table */
   C_MAKE(alloc_kind_table, alloc_kind_size, alloc_entry);
dlog(DEBUGALLOC,"init2.c: init_alloc step4\n");

   /* Access the table entry */
   table = alloc_kind_table;

   /* Scan the objects */
   for (i = 1; i < k_number; i++)
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
dlog(DEBUGALLOC,"init2.c: init_alloc step5\n");

   /*** Analyze monster allocation info ***/

   /* Clear the "aux" array */
   C_WIPE(&aux, MAX_LEVEL, s16b);

   /* Clear the "num" array */
   C_WIPE(&num, MAX_LEVEL, s16b);
dlog(DEBUGALLOC,"init2.c: init_alloc step6\n");

   /* Size of "alloc_race_table" */
   alloc_race_size = 0;

   /* Scan the monsters */
   for (i = 1; i < r_number_total; i++)
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
dlog(DEBUGALLOC,"init2.c: init_alloc step7\n");

   /* Collect the level indexes */
   for (i = 1; i < MAX_LEVEL; i++)
   {
      /* Group by level */
      num[i] += num[i-1];
   }

   /* Paranoia */
   if (!num[0]) quit("No town monsters!");

   /*** Initialize monster allocation info ***/
dlog(DEBUGALLOC,"init2.c: init_alloc step8\n");

   /* Allocate the alloc_race_table */
   C_MAKE(alloc_race_table, alloc_race_size, alloc_entry);

   /* Access the table entry */
   table = alloc_race_table;
dlog(DEBUGALLOC,"init2.c: init_alloc step9\n");

   /* Scan the monsters */
   for (i = 1; i < r_number_total; i++)
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
dlog(DEBUGALLOC,"init2.c: init_alloc step11 ending\n");

   /* Success */
   return (0);
}

/*
 * spells get a cost based on their qualities
 */
void make_spell_costs(void)
{
   s16b         i;
   object_kind *k_ptr;

   /* give the spells their cost */
   for (i = 1; i < k_number; i++)
   {
      /* Get the i'th object */
      k_ptr = &k_info[i];

/* if we come accross a spell, give that spell the correct value */
/* basically, this could be in k_info.txt, but this is a quicker 'hack' */
      if (k_ptr->tval == TV_SPELL)
      {
         spell_type *s_ptr = &s_info[k_ptr->sval];
         switch(s_ptr->type)
         {
            case SPELL_ATTACK_NAT:
            case SPELL_CHANGE_ITEM:
            case SPELL_CHANGE_WORLD: k_ptr->cost = (s32b) (s_ptr->level) *
                                                   (s32b) (s_ptr->scale) * 300L;
                                     if (!k_ptr->cost) k_ptr->cost = 300L;
                                     break;
            case SPELL_ESCAPE:
            case SPELL_SEEK:
            case SPELL_CHANGE_OTHER: k_ptr->cost = (s32b) (s_ptr->level) *
                                                   (s32b) (s_ptr->scale) * 500L;
                                     if (!k_ptr->cost) k_ptr->cost = 500L;
                                     break;

            case SPELL_CHANGE_SELF:
            case SPELL_ATTACK_DRK:
            case SPELL_HEAL:         k_ptr->cost = (s32b) (s_ptr->level) *
                                                   (s32b) (s_ptr->scale) * 800L;
                                     if (!k_ptr->cost) k_ptr->cost = 800L;
                                     break;
         }
      }
   }
}


/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
   s16b i;

   /* Initialize the "macro" package */
   (void)macro_init();

   /*** Prepare the "dungeon" information ***/

   /* Allocate and Wipe the object list */
   C_MAKE(i_list, MAX_I_IDX, object_type);

   /* Allocate and Wipe the spell-sets list */
   C_MAKE(s_list, MAX_SPELL_SET_IDX, spell_set_type);

   /* Allocate and Wipe the monster list */
   C_MAKE(mn_list, MAX_M_IDX, monster_type);

/* jk */
   C_MAKE(t_list, MAX_TR_IDX, trap_item_type);
   C_MAKE(is_list, MAX_IS_IDX, item_set_type);

   /* Allocate and wipe each line of the (main-level) cave */
   for (i = 0; i < MAX_HGT; i++)
   {
      /* Allocate one row of the cave */
      C_MAKE(dungeon.level[0][i], MAX_WID, cave_cell_type);
   }
   /* we use no sublevels as of now. */
   for (i = 1; i < MAX_SUB_LEVELS; i++)
   {
      dungeon.level_used[i] = FALSE;
   }

   /*** Prepare "vinfo" array ***/
dlog(DEBUGALLOC,"init2.c: init_other: before vinfo_init\n");
   /* Used by "update_view()" */
   (void)vinfo_init();
dlog(DEBUGALLOC,"init2.c: init_other: after vinfo_init\n");

   /*** Prepare the various "bizarre" arrays ***/


   /* Quark variables */
   C_MAKE(quark__str, QUARK_MAX, cptr);

   /*** Prepare the Player inventory ***/

   /* Allocate it */
   C_MAKE(inventory, INVEN_TOTAL, object_type);

dlog(DEBUGALLOC,"init2.c: init_other: before spell_costs\n");
   make_spell_costs();
dlog(DEBUGALLOC,"init2.c: init_other: after spell_costs\n");


/* jk - wipe the arena_visit_levels - when wizard commands add experience, */
/* visit levels were not initialized - and possibly in other cases too     */
   for (i = 0; i < PY_MAX_LEVEL; i++)
   {
      arena_visit_level[i] = 0;
   }

   /*** Pre-allocate the basic "auto-inscriptions" ***/

   /* The "basic" feelings */
   (void)quark_add("cursed");
   (void)quark_add("broken");
   (void)quark_add("average");
   (void)quark_add("good");

   /* The "extra" feelings */
   (void)quark_add("excellent");
   (void)quark_add("worthless");
   (void)quark_add("special");
   (void)quark_add("terrible");

   /* Some extra strings */
   (void)quark_add("uncursed");
   (void)quark_add("on sale");

   /*** Set the "default" options ***/

   /* Scan the options */
   for (i = 0; options[i].descr; i++)
   {
       /* Set the "default" options */
       if (options[i].variable) (*options[i].variable) = options[i].stdval;
   }

   /* Initialize the window flags */
   for (i = 0; i < 8; i++)
   {
      /* Assume no flags */
      op_ptr->window_flag[i] = 0L;
   }

   /*** Pre-allocate space for the "format()" buffer ***/

   /* Hack -- Just call the "format()" function */
   (void)format("%s", "thunder7@xs4all.nl");
dlog(DEBUGALLOC,"init2.c: init_other: step 2\n");

   /*** Pre-allocate space for save/restore screen ***/

   /* Save the screen */
   Term_save();

   /* Save the screen (embedded) */
   Term_save();

   /* Restore the screen (embedded) */
   Term_load();

   /* Restore the screen */
   Term_load();
dlog(DEBUGALLOC,"init2.c: init_other: step 3\n");

   /* Success */
   return (0);
}

/*
 * Initialize various Angband variables and arrays.
 *
 * This initialization involves the parsing of special files
 * in the "lib/data" and sometimes the "lib/edit" directories.
 *
 * Note that the "template" files are initialized first, since they
 * often contain errors.  This means that macros and message recall
 * and things like that are not available until after they are done.
 */
void init_some_arrays(void)
{
   /* Initialize feature info */
   note("[Initializing arrays... (features)]");
   if (init_f_info()) quit("Cannot initialize features");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_f_info done\n");

   /* Initialize object info */
   note("[Initializing arrays... (objects)]");
   if (init_k_info()) quit("Cannot initialize objects");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_f_info done\n");

   /* Initialize artifact info */
   note("[Initializing arrays... (artifacts)]");
   if (init_a_info()) quit("Cannot initialize artifacts");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_a_info done\n");

   /* Initialize ego-item info */
   note("[Initializing arrays... (ego-items)]");
   if (init_e_info()) quit("Cannot initialize ego-items");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_e_info done\n");

   /* Initialize monster info */
   note("[Initializing arrays... (monsters)]");
   if (init_r_info()) quit("Cannot initialize monsters");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_r_info done\n");

   /* Initialize feature info */
   note("[Initializing arrays... (vaults)]");
   if (init_v_info()) quit("Cannot initialize vaults");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_v_info done\n");

/* jk */
   /* Initialize trap info */
   note("[Initializing arrays... (traps)]");
   if (init_t_info()) quit("Cannot initialize traps");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_t_info done\n");

   /* Initialize spell info */
   note("[Initializing arrays... (spells)]");
   if (init_s_info()) quit("Cannot initialize spells");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_s_info done\n");

   /* Initialize some other arrays */
   note("[Initializing arrays...]");
   if (init_other()) quit("Cannot initialize arrays");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_other done\n");

   /* Initialize some other arrays */
   note("[Initializing arrays... (alloc)]");
   if (init_alloc()) quit("Cannot initialize alloc stuff");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: init_alloc done\n");
   /* Hack -- all done */
   note("[Initializing arrays... done]");

   /*** Load default user pref files ***/

   /* Initialize feature info */
   note("[Loading basic user pref file...]");

   /* Process that file */
dlog(DEBUGPREF,"init2.c: init_some_arrays: about to process pref.prf\n");
   (void)process_pref_file("pref.prf");
dlog(DEBUGPREF,"init2.c: init_some_arrays: all pref files processed\n");
   /* Done */
   note("[Initialization complete]");
dlog(DEBUGALLOC,"init2.c: init_some_arrays: done\n");
}



