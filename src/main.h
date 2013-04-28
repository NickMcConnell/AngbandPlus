/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* main.h: initialization functions for various platforms */

#ifndef INCLUDED_MAIN_H
#define INCLUDED_MAIN_H

#include "posband.h"

extern errr init_gtk(int argc, char **argv);
extern errr init_xaw(int argc, char **argv);
extern errr init_gcu(int argc, char **argv);
extern errr init_vcs(int argc, char **argv);
extern errr init_lfb(int argc, char **argv);
extern errr init_sdl(int argc, char **argv);

extern const char help_gtk[];
extern const char help_xaw[];
extern const char help_vcs[];
extern const char help_gcu[];
extern const char help_lfb[];
extern const char help_sdl[];


struct module
{
	cptr name;
	cptr help;
	errr (*init)(int argc, char **argv);
};

#endif /* INCLUDED_MAIN_H */
