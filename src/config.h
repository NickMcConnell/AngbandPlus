/* File: config.h */

/*
 * General game compiler options.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */


/*
 * Look through the following lines, and where a comment includes the
 * tag "OPTION:", examine the associated "#define" statements, and decide
 * whether you wish to keep, comment, or uncomment them.  You should not
 * have to modify any lines not indicated by "OPTION".
 *
 * Note: Also examine the "system" configuration file "h-config.h".  A
 * lot of options have been moved there.
 *
 * And finally, remember that the "Makefile" will specify some rather
 * important compile time options, like what visual module to use.
 */


/*
 * OPTION: Allow the use of "sound" in various places.
 */
#define USE_SOUND

/*
 * OPTION: Allow the use of "graphics" in various places
 */
#define USE_GRAPHICS


/*
 * OPTION: Gamma correct colours
 */
#define SUPPORT_GAMMA


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
/* #define ALLOW_SPOILERS */


/*
 * OPTION: Allow parsing of the ASCII template files in "init.c".
 * This must be defined if you do not have valid binary image files.
 * It should be usually be defined anyway to allow easy updating.
 */
#define ALLOW_TEMPLATES


/*
 * OPTION: Allow "Wizards" to yield "high scores"
 *
 * The only reason to allow SCORE_WIZARDS to be turned on is that
 * it can be handy when debugging the high score list.
 */
/* #define SCORE_WIZARDS */


/*
 * OPTION: Allow "Borgs" to yield "high scores"
 */
/* #define SCORE_BORGS */


/*
 * OPTION: Set the "default" path to the angband "lib" directory.
 *
 * See "main.c" for usage, and note that this value is only used on
 * certain machines, primarily UNIX machines.
 *
 * The configure script overrides this value.  Check the "--prefix=<dir>"
 * option of the configure script.
 *
 * This value will be over-ridden by the "SANGBAND_PATH" environment
 * variable, if that variable is defined and accessible.  The final
 * "slash" is required if the value supplied is in fact a directory.
 *
 * Using the value "./lib/" below tells Sangband that, by default,
 * the user will run "sangband" from the same directory that contains
 * the "lib" directory.  This is a reasonable (but imperfect) default.
 *
 * If at all possible, you should change this value to refer to the
 * actual location of the "lib" folder, for example, "/tmp/sangband/lib/"
 * or "/usr/games/lib/sangband/", or "/pkg/sangband/lib".
 */
#ifndef DEFAULT_PATH
# define DEFAULT_PATH "." PATH_SEP "lib" PATH_SEP
#endif


/*
 * OPTION: Create and use a hidden directory in the user's home directory
 * for storing pref-files and character-dumps.
 */
#ifdef SET_UID
# ifndef PRIVATE_USER_PATH
#  define PRIVATE_USER_PATH "~/.angband"
# endif /* PRIVATE_USER_PATH */
#endif /* SET_UID */


/*
 * OPTION: Create and use hidden directories in the user's home directory
 * for storing save files, data files, and high-scores
 */
#ifdef PRIVATE_USER_PATH
/* # define USE_PRIVATE_PATHS */
#endif /* PRIVATE_USER_PATH */

/*
 * Address of current maintainer.
 */
#define MAINTAINER_NAME "Leon Marrick"
#define MAINTAINER	"www.runegold.org/sangband"

