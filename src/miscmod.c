/* File: miscmod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides a Python interface to some miscellanous Angband variables
 * and functions.
 */

#include "angband.h"


/*
 * Method table for all functions provided by the module
 */
static PyMethodDef miscMethods[] =
{
	{ NULL, NULL }
};

/*
 * Helper function.  Add an integer to a dictionary.
 */
static void add_constant(PyObject *dict, char *name, int value)
{
	PyObject *temp;

	/* Form an object from the value */
	temp = Py_BuildValue("i", value);

	/* Set item */
	PyDict_SetItemString(dict, name, temp);

	/* Delete object */
	Py_DECREF(temp);
}

/*
 * Helper function.  Add a string to a dictionary.
 */
static void add_string(PyObject *dict, char *name, cptr value)
{
	PyObject *temp;

	/* Form an object from the value */
	temp = Py_BuildValue("s", value);

	/* Set item */
	PyDict_SetItemString(dict, name, temp);

	/* Delete object */
	Py_DECREF(temp);
}

/*
 * Initialize the misc module
 */
void initmisc(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
	m = Py_InitModule("misc", miscMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
	add_constant(d, "VERSION_MAJOR", VERSION_MAJOR);
	add_constant(d, "VERSION_MINOR", VERSION_MINOR);
	add_constant(d, "VERSION_PATCH", VERSION_PATCH);
	add_constant(d, "VERSION_EXTRA", VERSION_EXTRA);
	add_string(d, "VERSION_STRING", VERSION_STRING);
	add_string(d, "ANGBAND_SYS", ANGBAND_SYS);
	add_string(d, "ANGBAND_DIR", ANGBAND_DIR);
	add_string(d, "ANGBAND_DIR_APEX", ANGBAND_DIR_APEX);
	add_string(d, "ANGBAND_DIR_BONE", ANGBAND_DIR_BONE);
	add_string(d, "ANGBAND_DIR_DATA", ANGBAND_DIR_DATA);
	add_string(d, "ANGBAND_DIR_EDIT", ANGBAND_DIR_EDIT);
	add_string(d, "ANGBAND_DIR_FILE", ANGBAND_DIR_FILE);
	add_string(d, "ANGBAND_DIR_HELP", ANGBAND_DIR_HELP);
	add_string(d, "ANGBAND_DIR_INFO", ANGBAND_DIR_INFO);
	add_string(d, "ANGBAND_DIR_SAVE", ANGBAND_DIR_SAVE);
	add_string(d, "ANGBAND_DIR_SCPT", ANGBAND_DIR_SCPT);
	add_string(d, "ANGBAND_DIR_USER", ANGBAND_DIR_USER);
	add_string(d, "ANGBAND_DIR_XTRA", ANGBAND_DIR_XTRA);
}

