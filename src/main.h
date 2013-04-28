/* File: main.h */

/*
 * Copyright (c) 2007 Robert Ruehlmann
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#ifndef INCLUDED_MAIN_H
#define INCLUDED_MAIN_H

#include "angband.h"

extern errr init_gtk(int argc, char **argv);
extern errr init_x11(int argc, char **argv);
extern errr init_gcu(int argc, char **argv);
extern errr init_dos(int argc, char **argv);
extern errr init_ibm(int argc, char **argv);


extern const char help_gtk[];
extern const char help_x11[];
extern const char help_gcu[];
extern const char help_dos[];
extern const char help_ibm[];



struct module
{
	cptr name;
	cptr help;
	errr (*init)(int argc, char **argv);
};

#endif /* INCLUDED_MAIN_H */
