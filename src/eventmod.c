/* File: eventmod.c */

/*
 * Copyright (c) 1998 Keldon Jones
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * This file provides support for setting up Python functions to be called
 * whenever certain Angband "events" happen.
 *
 * This file also contains functions that should be called when events
 * happen.
 */

#include "angband.h"

/*
 * Table of callback functions
 */
static PyObject *python_callbacks[EVENT_MAX];

/*
 * Set an event callback
 */
static PyObject *event_add_handler(PyObject *self, PyObject *args)
{
	int type;
	PyObject *func;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iO", &type, &func))
		return NULL;

	/* Delete old callback, if any */
	Py_XDECREF(python_callbacks[type]);

	/* Add reference to the new one */
	Py_XINCREF(func);

	/* Remember callback */
	python_callbacks[type] = func;

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Method table for all functions provided by the module
 */
static PyMethodDef eventMethods[] =
{
	{ "add_handler", event_add_handler, 1 },
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
 * Initialize the event module
 */
void initevent(void)
{
	PyObject *m, *d;

	/* Tell the interpreter to insert the module */
	m = Py_InitModule("event", eventMethods);

	/* Add some symbolic constants to the module */
	d = PyModule_GetDict(m);

	/* Add each constant */
	add_constant(d, "EVENT_ENTER_LEVEL", EVENT_ENTER_LEVEL);
	add_constant(d, "EVENT_LEAVE_LEVEL", EVENT_LEAVE_LEVEL);
	add_constant(d, "EVENT_CREATE_LEVEL", EVENT_CREATE_LEVEL);
	add_constant(d, "EVENT_ATTACK", EVENT_ATTACK);
	add_constant(d, "EVENT_HURT_MONSTER", EVENT_HURT_MONSTER);
	add_constant(d, "EVENT_MOVE", EVENT_MOVE);
	add_constant(d, "EVENT_CAST", EVENT_CAST);
	add_constant(d, "EVENT_SUNRISE", EVENT_SUNRISE);
	add_constant(d, "EVENT_START_GAME", EVENT_START_GAME);
	add_constant(d, "EVENT_DIE", EVENT_DIE);
	add_constant(d, "EVENT_MONSTER_DEATH", EVENT_MONSTER_DEATH);
	add_constant(d, "EVENT_SAVE", EVENT_SAVE);
	add_constant(d, "EVENT_LOAD", EVENT_LOAD);
	add_constant(d, "EVENT_COMMAND", EVENT_COMMAND);
	add_constant(d, "EVENT_MONSTER_MOVE", EVENT_MONSTER_MOVE);
	add_constant(d, "EVENT_MONSTER_CAST", EVENT_MONSTER_CAST);
	add_constant(d, "EVENT_BIRTH_INFO", EVENT_BIRTH_INFO);
	add_constant(d, "EVENT_AIM_WAND", EVENT_AIM_WAND);
	add_constant(d, "EVENT_CLOSE_DOOR", EVENT_CLOSE_DOOR);
	add_constant(d, "EVENT_DROP", EVENT_DROP);
	add_constant(d, "EVENT_FIRE", EVENT_FIRE);
	add_constant(d, "EVENT_JAM", EVENT_JAM);
	add_constant(d, "EVENT_DESTROY", EVENT_DESTROY);
	add_constant(d, "EVENT_TARGET", EVENT_TARGET);
	add_constant(d, "EVENT_OPEN", EVENT_OPEN);
	add_constant(d, "EVENT_QUAFF", EVENT_QUAFF);
	add_constant(d, "EVENT_READ_SCROLL", EVENT_READ_SCROLL);
	add_constant(d, "EVENT_SEARCH", EVENT_SEARCH);
	add_constant(d, "EVENT_TAKEOFF", EVENT_TAKEOFF);
	add_constant(d, "EVENT_USE_STAFF", EVENT_USE_STAFF);
	add_constant(d, "EVENT_THROW", EVENT_THROW);
	add_constant(d, "EVENT_ZAP_ROD", EVENT_ZAP_ROD);
	add_constant(d, "EVENT_ACTIVATE", EVENT_ACTIVATE);
	add_constant(d, "EVENT_BASH", EVENT_BASH);
	add_constant(d, "EVENT_DISARM", EVENT_DISARM);
	add_constant(d, "EVENT_EAT", EVENT_EAT);
	add_constant(d, "EVENT_FILL", EVENT_FILL);
	add_constant(d, "EVENT_STUDY", EVENT_STUDY);
	add_constant(d, "EVENT_REST", EVENT_REST);
	add_constant(d, "EVENT_TUNNEL", EVENT_TUNNEL);
	add_constant(d, "EVENT_INSCRIBE", EVENT_INSCRIBE);
	add_constant(d, "EVENT_UNINSCRIBE", EVENT_UNINSCRIBE);
	add_constant(d, "EVENT_STORE", EVENT_STORE);
	add_constant(d, "EVENT_BONUSES", EVENT_BONUSES);
	add_constant(d, "EVENT_BOOK_OK", EVENT_BOOK_OK);
	add_constant(d, "EVENT_SPELL", EVENT_SPELL);
	add_constant(d, "EVENT_MAX", EVENT_MAX);
}

/*
 * Call a Python event handler, if it exists
 */
int perform_event(int type, PyObject *args)
{
	int retval = 0;
	PyObject *result;

	/* Check for existance of callback */
	if (python_callbacks[type])
	{
		/* Call function */
		result = PyEval_CallObject(python_callbacks[type], args);

		/* Check for error */
		if (!result)
		{
			/* Print error */
			PyErr_Print();
		}
		else
		{
			/* Check for no return */
			if (result == Py_None)
			{
				/* Assume 0 to prevent crashes */
				retval = 0;
			}
			else
			{
				/* Extract return value */
				retval = PyInt_AsLong(result);
			}

			/* Delete return object */
			Py_DECREF(result);
		}
	}

	/* Delete arguments */
	Py_XDECREF(args);

	/* Return */
	return (retval);
}
