/* File: init2.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Initialization (part 2) -BEN- */

#include "angband.h"

#include <assert.h>
#include "init.h"
#include "z-doc.h"
#include "dun.h"

#ifdef HAVE_STAT
#include <sys/stat.h>
#include <sys/types.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if defined (WINDOWS) && !defined (CYGWIN)
# define my_mkdir(path, perms) mkdir(path)
#elif defined(HAVE_MKDIR) || defined(MACH_O_CARBON) || defined (CYGWIN)
# define my_mkdir(path, perms) mkdir(path, perms)
#else
# define my_mkdir(path, perms) FALSE
#endif

/*
 * This file is used to initialize various variables and arrays for the
 * Angband game. Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/file" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass. Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * The "init1.c" file is used only to parse the ascii template files,
 * to create the binary image files. If you include the binary image
 * files instead of the ascii template files, then you can undefine
 * "ALLOW_TEMPLATES", saving about 20K by removing "init1.c". Note
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
 * or a system dependant string, for example, ":lib:". The buffer
 * must be large enough to contain at least 32 more characters.
 *
 * Various command line options may allow some of the important
 * directories to be changed to user-specified directories, most
 * importantly, the "info" and "user" and "save" directories,
 * but this is done after this function, see "main.c".
 *
 * In general, the initial path should end in the appropriate "PATH_SEP"
 * string. All of the "sub-directory" paths (created below or supplied
 * by the user) will NOT end in the "PATH_SEP" string, see the special
 * "path_build()" function in "util.c" for more information.
 *
 * Mega-Hack -- support fat raw files under NEXTSTEP, using special
 * "suffixed" directories for the "ANGBAND_DIR_DATA" directory, but
 * requiring the directories to be created by hand by the user.
 *
 * Hack -- first we free all the strings, since this is known
 * to succeed even if the strings have not been allocated yet,
 * as long as the variables start out as "NULL". This allows
 * this function to be called multiple times, for example, to
 * try several base "path" values until a good one is found.
 */
void init_file_paths(const char *configpath, const char *libpath, const char *datapath)
{
#ifdef NeXT
    char *tail;
#endif

#ifdef PRIVATE_USER_PATH
    char buf[1024];
#endif /* PRIVATE_USER_PATH */

    /*** Free everything ***/

    /* Free the main path */
    z_string_free(ANGBAND_DIR);

    /* Free the sub-paths */
    z_string_free(ANGBAND_DIR_SCORES);
    z_string_free(ANGBAND_DIR_BONE);
    z_string_free(ANGBAND_DIR_DATA);
    z_string_free(ANGBAND_DIR_EDIT);
    z_string_free(ANGBAND_DIR_SCRIPT);
    z_string_free(ANGBAND_DIR_FILE);
    z_string_free(ANGBAND_DIR_HELP);
    z_string_free(ANGBAND_DIR_INFO);
    z_string_free(ANGBAND_DIR_SAVE);
    z_string_free(ANGBAND_DIR_USER);
    z_string_free(ANGBAND_DIR_XTRA);


    /*** Prepare the "path" ***/

    /* Hack -- save the main directory */
    ANGBAND_DIR = z_string_make(libpath);

#ifdef NeXT
    /* Prepare to append to the Base Path */
    /* This is really suspicious code as we might sprintf to this buffer below! */
    tail = (char*)(libpath + strlen(libpath));
#endif

#ifdef VM

    /*** Use "flat" paths with VM/ESA ***/

    /* Use "blank" path names */
    ANGBAND_DIR_SCORES = z_string_make("");
    ANGBAND_DIR_BONE = z_string_make("");
    ANGBAND_DIR_DATA = z_string_make("");
    ANGBAND_DIR_EDIT = z_string_make("");
    ANGBAND_DIR_SCRIPT = z_string_make("");
    ANGBAND_DIR_FILE = z_string_make("");
    ANGBAND_DIR_HELP = z_string_make("");
    ANGBAND_DIR_INFO = z_string_make("");
    ANGBAND_DIR_SAVE = z_string_make("");
    ANGBAND_DIR_USER = z_string_make("");
    ANGBAND_DIR_XTRA = z_string_make("");


#else /* VM */


    /* Build path names */
    ANGBAND_DIR_EDIT = z_string_make(format("%sedit", configpath));
    ANGBAND_DIR_FILE = z_string_make(format("%sfile", libpath));
    ANGBAND_DIR_HELP = z_string_make(format("%shelp", libpath));
    ANGBAND_DIR_INFO = z_string_make(format("%sinfo", libpath));
    ANGBAND_DIR_PREF = z_string_make(format("%spref", configpath));
    ANGBAND_DIR_XTRA = z_string_make(format("%sxtra", libpath));

    /*** Build the sub-directory names ***/

#ifdef PRIVATE_USER_PATH

    /* Build the path to the user specific directory */
    path_build(buf, sizeof(buf), PRIVATE_USER_PATH, VERSION_NAME);

    /* Build a relative path name */
    ANGBAND_DIR_USER = z_string_make(buf);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "scores");
    ANGBAND_DIR_SCORES = z_string_make(buf);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "bone");
    ANGBAND_DIR_BONE = z_string_make(buf);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "data");
    ANGBAND_DIR_DATA = z_string_make(buf);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "script");
    ANGBAND_DIR_SCRIPT = z_string_make(buf);

    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "save");
    ANGBAND_DIR_SAVE = z_string_make(buf);

#else /* PRIVATE_USER_PATH */

    /* Build pathnames */
    ANGBAND_DIR_USER = z_string_make(format("%suser", datapath));
    ANGBAND_DIR_SCORES = z_string_make(format("%sscores", datapath));
    ANGBAND_DIR_BONE = z_string_make(format("%sbone", datapath));
    ANGBAND_DIR_DATA = z_string_make(format("%sdata", datapath));
    ANGBAND_DIR_SCRIPT = z_string_make(format("%sscript", datapath));
    ANGBAND_DIR_SAVE = z_string_make(format("%ssave", datapath));

#endif /* PRIVATE_USER_PATH */

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
            z_string_free(ANGBAND_DIR_DATA);

            /* Build a new path name */
            sprintf(tail, "data-%s", next);
            ANGBAND_DIR_DATA = z_string_make(libpath);
        }
    }

#endif /* NeXT */

}

bool dir_exists(const char *path)
{
    #ifdef HAVE_STAT
    struct stat buf;
    if (stat(path, &buf) != 0)
        return FALSE;
    else if (buf.st_mode & S_IFDIR)
        return TRUE;
    else
        return FALSE;
    #else
    return TRUE;
    #endif
}

#ifdef HAVE_STAT
bool dir_create(const char *path)
{
    const char *ptr;
    char buf[512];

    /* If the directory already exists then we're done */
    if (dir_exists(path)) return TRUE;

    #ifdef WINDOWS
    /* If we're on windows, we need to skip past the "C:" part. */
    if (isalpha(path[0]) && path[1] == ':') path += 2;
    #endif

    /* Iterate through the path looking for path segements. At each step,
     * create the path segment if it doesn't already exist. */
    for (ptr = path; *ptr; ptr++) {
        if (*ptr == PATH_SEPC) {
            /* Find the length of the parent path string */
            size_t len = (size_t)(ptr - path);

            /* Skip the initial slash */
            if (len == 0) continue;

            /* If this is a duplicate path separator, continue */
            if (*(ptr - 1) == PATH_SEPC) continue;

            /* We can't handle really big filenames */
            if (len - 1 > 512) return FALSE;

            /* Create the parent path string, plus null-padding */
            my_strcpy(buf, path, len + 1);

            /* Skip if the parent exists */
            if (dir_exists(buf)) continue;

            /* The parent doesn't exist, so create it or fail */
            if (my_mkdir(buf, 0755) != 0) return FALSE;
        }
    }
    return my_mkdir(path, 0755) == 0 ? TRUE : FALSE;
}
#else /* HAVE_STAT */
bool dir_create(const char *path) { return FALSE; }
#endif /* !HAVE_STAT */


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

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_SCORES, "");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_SCORES, "html");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_DATA, "");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_HELP, "");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_FILE, "screen");
    if (!dir_create(dirpath)) quit_fmt("Cannot create '%s'", dirpath);

    path_build(dirpath, sizeof(dirpath), ANGBAND_DIR_HELP, "screen");
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
    "coordinates out of bounds",
    "too few arguments",
    "undefined terrain tag",

};


/*
 * File headers
 */
header f_head;
header k_head;
header a_head;
header e_head;
header r_head;
header d_head;
header s_head;
header m_head;
header b_head;

/*
 * Initialize the header of an *_info.raw file.
 */
static void init_header(header *head, int num, int len)
{
    /* Save the "version" */
    head->v_major = VER_MAJOR;
    head->v_minor = VER_MINOR;
    head->v_patch = VER_PATCH;
    head->v_extra = 0;

    /* Save the "record" information */
    head->info_num = num;
    head->info_len = len;

    /* Save the size of "*_head" and "*_info" */
    head->head_size = sizeof(header);
    head->info_size = head->info_num * head->info_len;
}


/*
 * Initialize the "*_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 *
 * Note: We used to read *.raw files for binary info. Optionally, we
 * would parse txt template files to recreate the raw files, throw
 * everything away, and re-read the raw files. Alas, the code was not
 * good at knowing when to recreate (it used timestamps) and often
 * resulted in corruption. Now, while I can certainly figure this out
 * and manually delete my *.raw files, I'd rather not have player's games
 * crash when they upgrade to a new version. Parsing templates takes 150ms
 * on my ancient machine, so why bother?
 *
 * TODO: Refactor Initialization code. No need for headers, etc.
 * Tags, Text and Name should just be malloc'd strings. Room templates
 * should support a variable number of custom 'letters', etc.
 */
static errr init_info(cptr filename, header *head,
              void **info, char **name, char **text, char **tag)
{
    errr err = 1;
    FILE *fp;

    /* General buffer */
    char buf[1024];


    /* Allocate the "*_info" array */
    C_MAKE(head->info_ptr, head->info_size, char);

    /* Hack -- make "fake" arrays */
    if (name) C_MAKE(head->name_ptr, FAKE_NAME_SIZE, char);
    if (text) C_MAKE(head->text_ptr, FAKE_TEXT_SIZE, char);
    if (tag)  C_MAKE(head->tag_ptr, FAKE_TAG_SIZE, char);

    if (info) (*info) = head->info_ptr;
    if (name) (*name) = head->name_ptr;
    if (text) (*text) = head->text_ptr;
    if (tag)  (*tag)  = head->tag_ptr;

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
    if (err)
    {
        cptr oops;

        /* Error string */
        oops = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

        /* Oops */
        msg_format("Error %d at line %d of '%s.txt'.", err, error_line, filename);
        msg_format("Record %d contains a '%s' error.", error_idx, oops);
        msg_format("Parsing '%s'.", buf);
        msg_print(NULL);

        /* Quit */
        quit(format("Error in '%s.txt' file.", filename));

    }


    /*** Make final retouch on fake tags ***/

    if (head->retouch)
    {
        (*head->retouch)(head);
    }

    /* Success */
    return (0);
}


/*
 * Initialize the "k_info" array
 */
static errr init_k_info(void)
{
    /* Init the header */
    init_header(&k_head, max_k_idx, sizeof(object_kind));


    /* Save a pointer to the parsing function */
    k_head.parse_info_txt = parse_k_info;


    return init_info("k_info", &k_head,
             (void*)&k_info, NULL, NULL, NULL);
}



/*
 * Initialize the "e_info" array
 */
static errr init_e_info(void)
{
    /* Init the header */
    init_header(&e_head, max_e_idx, sizeof(ego_type));


    /* Save a pointer to the parsing function */
    e_head.parse_info_txt = parse_e_info;


    return init_info("e_info", &e_head,
             (void*)&e_info, NULL, NULL, NULL);
}

/*
 * Initialize the "s_info" array
 */
static errr init_s_info(void)
{
    /* Init the header */
    init_header(&s_head, MAX_CLASS, sizeof(skill_table));


    /* Save a pointer to the parsing function */
    s_head.parse_info_txt = parse_s_info;


    return init_info("s_info", &s_head,
             (void*)&s_info, NULL, NULL, NULL);
}


/*
 * Initialize the "m_info" array
 */
static errr init_m_info(void)
{
    /* Init the header */
    init_header(&m_head, MAX_CLASS, sizeof(player_magic));


    /* Save a pointer to the parsing function */
    m_head.parse_info_txt = parse_m_info;


    return init_info("m_info", &m_head,
             (void*)&m_info, NULL, NULL, NULL);
}




/*
 * Initialize misc. values
 */
static errr _parse_misc(char *line, int options)
{
    char *zz[2];
    if (tokenize(line, 2, zz, 0) == 2)
    {
        if (zz[0][0] == 'K')
            max_k_idx = atoi(zz[1]);
        else if (zz[0][0] == 'F')
            max_f_idx = atoi(zz[1]);
        else if (zz[0][0] == 'E')
            max_e_idx = atoi(zz[1]);
        return 0;
    }
    return PARSE_ERROR_GENERIC;
}

static errr init_misc(void)
{
    return parse_edit_file("misc.txt", _parse_misc, 0);
}

/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
    int i, n;

    /*** Prepare the various "bizarre" arrays ***/

    /* Macro variables */
    C_MAKE(macro__pat, MACRO_MAX, cptr);
    C_MAKE(macro__act, MACRO_MAX, cptr);
    C_MAKE(macro__cmd, MACRO_MAX, bool);
    macro__num = 0;

    /* Macro action buffer */
    C_MAKE(macro__buf, 1024, char);

    /* Quark variables */
    quark_init();

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
                window_mask[n] |= (1L << i);
            }
        }
    }


    /*** Pre-allocate space for the "format()" buffer ***/

    /* Hack -- Just call the "format()" function */
    (void)format("%s (%s).", VERSION_NAME, "Hack Whack");


    /* Success */
    return (0);
}


/*
 * Initialize some other arrays
 */
static errr init_object_alloc(void)
{
    int i, j;
    object_kind *k_ptr;
    alloc_entry *table;
    s16b num[MAX_DEPTH];
    s16b aux[MAX_DEPTH];


    /*** Analyze object allocation info ***/

    /* Clear the "aux" array */
    (void)C_WIPE(&aux, MAX_DEPTH, s16b);

    /* Clear the "num" array */
    (void)C_WIPE(&num, MAX_DEPTH, s16b);

    /* Free the old "alloc_kind_table" (if it exists) */
    if (alloc_kind_table)
    {
        C_KILL(alloc_kind_table, alloc_kind_size, alloc_entry);
    }

    /* Size of "alloc_kind_table" */
    alloc_kind_size = 0;

    /* Scan the objects */
    for (i = 1; i < max_k_idx; i++)
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
    for (i = 1; i < max_k_idx; i++)
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
                table[z].max_level = k_ptr->max_level;

                /* Another entry complete for this locale */
                aux[x]++;
            }
        }
    }

    /* Success */
    return (0);
}


/*
 * Initialize some other arrays
 */
static errr init_alloc(void)
{
    mon_alloc_init();

    /* Init the "alloc_kind_table" */
    (void)init_object_alloc();

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
static void init_angband_quit(cptr why)
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

/************************************************************************
 * News: Display a (graphical) startup screen to whet the appetite
 *
 * screenshots are placed in ./lib/file/screen as news%2d.old|ascii
 * ./lib/file/screen/count.old|ascii gives the number of start screens,
 * and one is randomly chosen for display.
 *
 * Screenshots change every release and are not under version control.
 * Use ^] to create screen.old in ANGBAND_DIR_USER. Under X11, use mouse
 * to select a rectangular region and then right-click to create screen.old
 * in ANGBAND_DIR_USER.
 ************************************************************************/
static FILE *_news_fopen(cptr name)
{
    char buf1[1024], buf2[1024];
    path_build(buf1, sizeof(buf1), ANGBAND_DIR_FILE, "screen");
    path_build(buf2, sizeof(buf2), buf1, name);
    return my_fopen(buf2, "r");
}
static int _news_count(void)
{
    FILE *fp = _news_fopen(format("count.%s", ANGBAND_GRAF));
    int count = 1;
    if (!fp) return count;
    fscanf(fp, "%d", &count);
    fclose(fp);
    return count;
}
static void _term_erase(doc_ptr doc)
{
    int y;
    for (y = doc_cursor(doc).y; y < Term->hgt - 1; y++)
        Term_erase(0, y, Term->wid);
}
static void _display_screen_old(FILE *fp)
{
    doc_ptr doc = doc_alloc(120);
    doc_read_term(doc, fp);
    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 1));
    _term_erase(doc);
    doc_free(doc);
}
static void _display_screen_doc(FILE *fp)
{
    doc_ptr doc = doc_alloc(120);
    doc_read_file(doc, fp);
    doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 1));
    _term_erase(doc);
    doc_free(doc);
}
static void _display_screen(cptr name)
{
    FILE *fp = 0;
    char buf[100];

    c_prt(TERM_YELLOW,
        format("%s (%d.%d.%d)", VERSION_NAME, VER_MAJOR, VER_MINOR, VER_PATCH),
        0, 0);
    if (strcmp(ANGBAND_GRAF, "old") == 0) /* try for graphics */
    {
        sprintf(buf, "%s.old", name);
        fp = _news_fopen(buf);
        if (fp)
            _display_screen_old(fp);
    }
    if (!fp) /* fallback to ascii */
    {
        sprintf(buf, "%s.doc", name);
        fp = _news_fopen(buf);
        if (fp)
            _display_screen_doc(fp);
    }
    if (!fp) /* XXX */
    {
    }
    my_fclose(fp);
    c_prt(TERM_YELLOW, "              [Press ? for Credits. Press Any Other Key to Play]", Term->hgt - 1, 0);
    Term_fresh();
}
static void _display_file(cptr name)
{
    FILE *fp;
    char buf[1024];

    Term_clear();
    path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, name);
    fp = my_fopen(buf, "r");

    if (fp)
    {
        int w, h;
        doc_ptr doc;

        Term_get_size(&w, &h);
        doc = doc_alloc(100);
        doc_read_file(doc, fp);
        doc_sync_term(doc, doc_range_all(doc), doc_pos_create(0, 0));
        doc_free(doc);
        my_fclose(fp);

        c_prt(TERM_YELLOW, "              [Press ? for Credits. Press Any Other Key to Play]", Term->hgt - 1, 0);
        Term_fresh();
    }
}

void display_news(void)
{
    int max_n = _news_count();
    int n;
    bool done = FALSE;

    srand(time(NULL));
    n = (rand() % max_n) + 1;

    while (!done)
    {
        char name[100];
        int  cmd;

        sprintf(name, "news%02d", n);
        _display_screen(name);

        /* Windows is an odd duck, indeed! User must select a menu item to continue. */
        if (strcmp(ANGBAND_SYS, "win") == 0)
            break;

        cmd = inkey_special(TRUE);
        switch (cmd)
        {
        case '?':
            _display_file("credits.txt");
            inkey();
            break;
        case SKEY_DOWN:
        case '2':
            n++;
            if (n > max_n)
                n = 1;
            break;
        case SKEY_UP:
        case '8':
            n--;
            if (n == 0)
                n = max_n;
            break;
        default:
            done = TRUE;
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
 * correctly located. Otherwise, the news file is displayed for
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
 * often contain errors. This means that macros and message recall
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
    char buf[1024];

    /* tell me when "flags" need more space */
    assert(OF_COUNT <= OF_ARRAY_SIZE * 32);
    assert(PAF_COUNT <= PAF_ARRAY_SIZE * 32);
    assert(OF_COUNT < 240); /* XXX cf _MIN_SPECIAL in sword|smith ... these need a re-write! */

    temp_pts = point_vec_alloc();

    /* XXX needs to be first */
    plr_startup();

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
        sprintf(why, "Cannot access the '%s' file!", buf);


        /* Crash and burn */
        init_angband_quit(why);
    }

    /* Close it */
    (void)fd_close(fd);

    msg_startup();
    sym_startup();

    /*** Initialize some arrays ***/

    /* Initialize misc. values */
    note("[Initializing values... (misc)]");

    if (init_misc()) quit("Cannot initialize misc. values");

    /* Initialize feature info */
    note("[Initializing arrays... (features)]");
    if (!dun_feat_init()) quit("Cannot initialize features");

    /* Initialize object info */
    note("[Initializing arrays... (objects)]");
    if (init_k_info()) quit("Cannot initialize objects");

    /* Initialize artifact info */
    note("[Initializing arrays... (artifacts)]");
    if (!arts_init()) quit("Cannot initialize artifacts");

    /* Initialize ego-item info */
    note("[Initializing arrays... (ego-items)]");
    if (init_e_info()) quit("Cannot initialize ego-items");

    /* Initialize monster info */
    note("[Initializing arrays... (body-types)]");
    if (!equip_template_init()) quit("Cannot initialize equipment templates");

    note("[Initializing arrays... (monk attacks)]");
    if (!monk_attack_init()) quit("Cannot initialize monk attacks");

    /* Initialize monster info ... requires b_info, k_info, a_info, e_info, monk_attacks */
    note("[Initializing arrays... (monsters)]");
    if (!mon_race_init()) quit("Cannot initialize monsters");

    /* Initialize magic info */
    note("[Initializing arrays... (magic)]");
    if (init_m_info()) quit("Cannot initialize magic");

    /* Initialize weapon_exp info */
    note("[Initializing arrays... (skill)]");
    if (init_s_info()) quit("Cannot initialize skill");

    note("[Initializing arrays... (quests)]");
    if (!quests_init()) quit("Cannot initialize quests");

    /* Initialize vault info */
    if (init_v_info(0)) quit("Cannot initialize vaults");
    if (parse_n_choose_k()) quit("Cannot initialize line generator");

    /* Initialize some other arrays */
    note("[Initializing arrays... (other)]");
    if (init_other()) quit("Cannot initialize other stuff");

    /* Initialize some other arrays */
    note("[Initializing arrays... (alloc)]");
    if (init_alloc()) quit("Cannot initialize alloc stuff");

    /*** Load default user pref files ***/

    /* Initialize feature info */
    note("[Initializing user pref files...]");

    /* Access the "basic" pref file */
    strcpy(buf, "pref.prf");

    /* Process that file */
    process_pref_file(buf);

    /* Access the "basic" system pref file */
    sprintf(buf, "pref-%s.prf", ANGBAND_SYS);

    /* Process that file */
    process_pref_file(buf);

    /* Done */
    note("[Initialization complete]");

    /* We are now initialized */
    initialized = TRUE;
}

