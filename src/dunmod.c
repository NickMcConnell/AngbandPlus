/* File: dunmod.c */

#ifdef USE_PYTHON

/*
 * Copyright (c) 1999 Dark God
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
 * Return the dungeon level
 */
static PyObject *dungeon_dun_level(PyObject *self, PyObject *args)
{
	/* Parse arguments */
        if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Return random number */
        return Py_BuildValue("i", dun_level);
}

/*
 * Return the dungeon type
 */
static PyObject *dungeon_dungeon_type(PyObject *self, PyObject *args)
{
	/* Parse arguments */
        if (!PyArg_ParseTuple(args, ""))
		return NULL;

	/* Return random number */
        return Py_BuildValue("i", dungeon_type);
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef dungeonMethods[] =
{
        { "dun_level", dungeon_dun_level, 1 },
        { "dungeon_type", dungeon_dungeon_type, 1 },
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
 * Initialize the dungeon module
 */
void initdungeon(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
        m = Py_InitModule("dungeon", dungeonMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
        add_constant(d, "DUNGEON_WILDERNESS", DUNGEON_WILDERNESS);
        add_constant(d, "DUNGEON_MIRKWOOD", DUNGEON_MIRKWOOD);
        add_constant(d, "DUNGEON_MORDOR", DUNGEON_MORDOR);
        add_constant(d, "DUNGEON_ANGBAND", DUNGEON_ANGBAND);
        add_constant(d, "DUNGEON_GALGALS", DUNGEON_GALGALS);
        add_constant(d, "DUNGEON_VOLCANO", DUNGEON_VOLCANO);
        add_constant(d, "DUNGEON_HELL", DUNGEON_HELL);
}

#endif
