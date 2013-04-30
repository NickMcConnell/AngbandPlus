/*
 * File: config.h
 * Purpose: Angband specific configuration stuff
 */

#ifndef INCLUDED_CONFIG_H
#define INCLUDED_CONFIG_H

/*** Some really important things you ought to change ***/

/*
 * Defines the default paths to the Angband directories, for ports that use
 * the main.c file.
 *
 * "config path" is for per-installation configurable data, like the game's
 * edit files and system-wide preferences.
 *
 * "lib path" is for static data, like sounds, graphics and fonts.
 *
 * "data path" is for variable data, like save files and scores. On single-
 * user systems, this also includes user preferences and dumps (on multi-
 * user systems these go under the user's home directory).
 *
 * Using the value "./lib/" below tells Angband that, by default,
 * the user will run "angband" from the same directory that contains
 * the "lib" directory.  This is a reasonable (but imperfect) default.
 *
 * If at all possible, you should change this value to refer to the
 * actual location of the folders, for example, "/etc/angband/"
 * or "/usr/share/angband/", or "/var/games/angband/". In fact, if at all
 * possible you should use a packaging system which does this for you.
 */
#ifndef DEFAULT_CONFIG_PATH
# define DEFAULT_CONFIG_PATH "." PATH_SEP "lib" PATH_SEP
#endif

#ifndef DEFAULT_LIB_PATH
# define DEFAULT_LIB_PATH "." PATH_SEP "lib" PATH_SEP
#endif

#ifndef DEFAULT_DATA_PATH
# define DEFAULT_DATA_PATH "." PATH_SEP "lib" PATH_SEP
#endif

/*** Some no-brainer defines ***/

/* Allow the game to make noises correlating to what the player does in-game */
#define USE_SOUND

/* Allow the use of graphics rather than only having a text-mode */
#define USE_GRAPHICS

/*** Things useful for debugging compiles ***/

/* Compile in support for debug commands */
/* #define _DEBUG */

/*** Other defines ***/

/*
 * OPTION: Do not switch to manual targeting if there are no
 * targets in the vicinity of player (un-Angband), display a prompt instead
 */
#define NOTARGET_PROMPT

#endif /* INCLUDED_CONFIG_H */
