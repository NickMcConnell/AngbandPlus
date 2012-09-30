/* File: questmod.c */

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
 * Return the quest status
 */
static PyObject *quest_status(PyObject *self, PyObject *args)
{
        int n;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "i", &n))
		return NULL;

	/* Return random number */
        return Py_BuildValue("i", quest[n].status);
}

/*
 * Set the quest status
 */
static PyObject *quest_set_status(PyObject *self, PyObject *args)
{
        int n, s;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "ii", &n, &s))
		return NULL;

        quest[n].status = s;

	/* Return random number */
        return Py_None;
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef questMethods[] =
{
        { "status", quest_status, 1 },
        { "set_status", quest_set_status, 1 },
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
 * Initialize the quest module
 */
void initquest(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
        m = Py_InitModule("quest", questMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
        add_constant(d, "QUEST_STATUS_UNTAKEN", QUEST_STATUS_UNTAKEN);
        add_constant(d, "QUEST_STATUS_TAKEN", QUEST_STATUS_TAKEN);
        add_constant(d, "QUEST_STATUS_COMPLETED", QUEST_STATUS_COMPLETED);
        add_constant(d, "QUEST_STATUS_REWARDED", QUEST_STATUS_REWARDED);
        add_constant(d, "QUEST_STATUS_FINISHED", QUEST_STATUS_FINISHED);
        add_constant(d, "QUEST_STATUS_FAILED", QUEST_STATUS_FAILED);
        add_constant(d, "QUEST_STATUS_FAILED_DONE", QUEST_STATUS_FAILED_DONE);
        add_constant(d, "QUEST_TYPE_KILL_LEVEL", QUEST_TYPE_KILL_LEVEL);
        add_constant(d, "QUEST_TYPE_KILL_ANY_LEVEL", QUEST_TYPE_KILL_ANY_LEVEL);
        add_constant(d, "QUEST_TYPE_FIND_ARTIFACT", QUEST_TYPE_FIND_ARTIFACT);
        add_constant(d, "QUEST_TYPE_FIND_EXIT", QUEST_TYPE_FIND_EXIT);
        add_constant(d, "QUEST_TYPE_KILL_NUMBER", QUEST_TYPE_KILL_NUMBER);
        add_constant(d, "QUEST_TYPE_KILL_ALL", QUEST_TYPE_KILL_ALL);
        add_constant(d, "QUEST_TYPE_RANDOM", QUEST_TYPE_RANDOM);
}

