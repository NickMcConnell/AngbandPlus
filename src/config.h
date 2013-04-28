/* File: config.h */

/*
 * General game compiler options.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


/*
 * Look through the following lines, and where a comment includes the
 * tag "OPTION:", examine the associated "#define" statements, and decide
 * whether you wish to keep, comment, or uncomment them.  You should not
 * have to modify any lines not indicated by "OPTION".
 *
 * A number of compile options have been removed in Sangband, usually for
 * one of three reasons:
 * 1) The code in question has been fully incorporated into the game,
 *   like monster fear, the DRS monster memory code, and Tim Baker's easy
 *   patch.
 * 2) The feature no longer imposes any significant costs (monster terror).
 * 3) The game no longer supports x286-based machines (because of the use
 *   of 64K arrays to hold monster descriptions, and because the code now
 *   runs very slowly on them).
 *
 * Note: Also examine the "system" configuration file "h-config.h".  A
 * lot of options have been moved there.
 *
 * And finally, remember that the "Makefile" will specify some rather
 * important compile time options, like what visual module to use.
 */



/*
 * OPTION: Verify savefile Checksums (Angband 2.7.0 and up)
 * This option can help prevent "corruption" of savefiles, and also
 * stop intentional modification by amateur users.
 */
#define VERIFY_CHECKSUMS


/*
 * OPTION: Forbid the use of "fiddled" savefiles.  As far as I can tell,
 * a fiddled savefile is one with an internal timestamp different from
 * the actual timestamp.  Thus, turning this option on forbids one from
 * copying a savefile to a different name.  Combined with disabling the
 * ability to save the game without quitting, and with some method of
 * stopping the user from killing the process at the tombstone screen,
 * this should prevent the use of backup savefiles.  It may also stop
 * the use of savefiles from other platforms, so be careful.
 */
/* #define VERIFY_TIMESTAMP */


/*
 * OPTION: Forbid the "savefile over-write" cheat, in which you simply
 * run another copy of the game, loading a previously saved savefile,
 * and let that copy over-write the "dead" savefile later.  This option
 * either locks the savefile, or creates a fake "xxx.lok" file to prevent
 * the use of the savefile until the file is deleted.  Not ready yet.
 */
/* #define VERIFY_SAVEFILE */



/*
 * OPTION: Hack -- Compile in support for "Borg mode"
 */
/* #define ALLOW_BORG */

/*
 * OPTION: Hack -- Compile in support for "Debug Commands"
 */
#define ALLOW_DEBUG

/*
 * OPTION: Hack -- Compile in support for "Spoiler Generation"
 */
#define ALLOW_SPOILERS


/*
 * OPTION: Allow "do_cmd_colors" at run-time
 */
#define ALLOW_COLORS

/*
 * OPTION: Allow "do_cmd_visuals" at run-time
 */
#define ALLOW_VISUALS

/*
 * OPTION: Allow "do_cmd_macros" at run-time
 */
#define ALLOW_MACROS


/*
 * OPTION: Allow characters to be "auto-rolled"
 */
#define ALLOW_AUTOROLLER


/*
 * OPTION: Allow parsing of the ASCII template files in "init.c".
 * This must be defined if you do not have valid binary image files.
 * It should be usually be defined anyway to allow easy updating.
 */
#define ALLOW_TEMPLATES

/*
 * OPTION: Delay the loading of the "f_text" array until it is actually
 * needed, saving ~1K, since "feature" descriptions are unused.
 */
#define DELAY_LOAD_F_TEXT

/*
 * OPTION: Delay the loading of the "k_text" array until it is actually
 * needed, saving ?K, but slowing down the display of object descriptions.
 */
/* #define DELAY_LOAD_K_TEXT */

/*
 * OPTION: Delay the loading of the "a_text" array until it is actually
 * needed, saving ?K, but slowing down the display of artifact descriptions.
 */
/* #define DELAY_LOAD_A_TEXT */

/*
 * OPTION: Delay the loading of the "e_text" array until it is actually
 * needed, saving ?K, but slowing down the display of ego-item descriptions.
 */
/* #define DELAY_LOAD_E_TEXT */

/*
 * OPTION: Delay the loading of the "r_text" array until it is actually
 * needed, saving ?K, but "simplifying" the "monster" descriptions.
 */
/* #define DELAY_LOAD_R_TEXT */

/*
 * OPTION: Delay the loading of the "v_text" array until it is actually
 * needed, saving ?K, but "destroying" the "vault" generation.
 */
/* #define DELAY_LOAD_V_TEXT */


/*
 * OPTION: Allow "Wizards" to yield "high scores"
 */
/* #define SCORE_WIZARDS */

/*
 * OPTION: Allow "Borgs" to yield "high scores"
 */
/* #define SCORE_BORGS */

/*
 * OPTION: Allow "Cheaters" to yield "high scores"
 */
/* #define SCORE_CHEATERS */



/*
 * OPTION: Allow monsters to use noise and scent information to better
 * track the character.  This feature requires a significant amount of
 * memory, but makes monsters behave much more intelligently.
 */
#define MONSTER_FLOW


/*
 * OPTION: Gamma correct colours (with X11)
 */
#define SUPPORT_GAMMA


/*
 * OPTION: Check the modification time of *.raw files
 */
#define CHECK_MODIFICATION_TIME


/*
 * OPTION: Allow the use of "sound" in various places.
 */
#define USE_SOUND

/*
 * OPTION: Allow the use of "graphics" in various places
 */
#define USE_GRAPHICS




/*
 * OPTION: Set the "default" path to the angband "lib" directory.
 *
 * See "main.c" for usage, and note that this value is only used on
 * certain machines, primarily UNIX machines.
 *
 * The configure script overrides this value.  Check the "--prefix=<dir>"
 * option of the configure script.
 *
 * This value will be over-ridden by the "ANGBAND_PATH" environment
 * variable, if that variable is defined and accessible.  The final
 * "slash" is required if the value supplied is in fact a directory.
 *
 * Using the value "./lib/" below tells Angband that, by default,
 * the user will run "angband" from the same directory that contains
 * the "lib" directory.  This is a reasonable (but imperfect) default.
 *
 * If at all possible, you should change this value to refer to the
 * actual location of the "lib" folder, for example, "/tmp/angband/lib/"
 * or "/usr/games/lib/angband/", or "/pkg/angband/lib".
 */
#ifndef DEFAULT_PATH
# define DEFAULT_PATH "." PATH_SEP "lib" PATH_SEP
#endif



/*
 * OPTION: Check the "time" against "lib/file/hours.txt"
 */
/* #define CHECK_TIME */

/*
 * OPTION: Check the "load" against "lib/file/load.txt"
 * This may require the 'rpcsvs' library
 */
/* #define CHECK_LOAD */


/*
 * OPTION: Some advanced computers have bid "good riddance" to the command
 * line interface, and make it easy to see and manipulate your own files.
 * The finest example of putting the user, not the rules, in charge is
 * Macintosh, which allows you to name files just about anything and save
 * them just about anywhere you please.  Allow the fortunate users of such
 * machines the ability to rename savefiles when the character name does.
 *
 * You thought that was a little jingoistic?  Check this out:
 *
 * OPTION: For some brain-dead computers with no command line interface,
 * namely Macintosh, there has to be some way of "naming" your savefiles.
 * The current "Macintosh" hack is to make it so whenever the character
 * name changes, the savefile is renamed accordingly.  But on normal
 * machines, once you manage to "load" a savefile, it stays that way.
 * Macintosh is particularly weird because you can load savefiles that
 * are not contained in the "lib:save:" folder, and if you change the
 * player's name, it will then save the savefile elsewhere.  Note that
 * this also gives a method of "bypassing" the "VERIFY_TIMESTAMP" code.
 *
 * Leaving the verbiage aside now:
 *
 * OPTION:  Change savefile names when the character's name does.  In
 * Sangband, this is not recommended (in any port), because it confuses
 * the system-independant savefile management system.  The game should
 * be able to handle things if you do define this, however, especially if
 * your port uses menus.
 */
/* #define SAVEFILE_MUTABLE */




/*
 * OPTION: Address to contact if something goes wrong.
 */
#define MAINTAINER_NAME "Leon Marrick"
#define MAINTAINER	"leon2m@sprintmail.com"



/*
 * OPTION: Attempt to prevent all "cheating"
 */
/* #define VERIFY_HONOR */


/*
 * React to the "VERIFY_HONOR" flag
 */
#ifdef VERIFY_HONOR
# define VERIFY_SAVEFILE
# define VERIFY_CHECKSUMS
# define VERIFY_TIMESTAMP
#endif


