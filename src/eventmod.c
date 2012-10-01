/* File: eventmod.c */

#ifdef USE_PYTHON

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
static PyObject *python_callbacks[EVENT_MAX][MAX_HANDLER];
static python_callbacks_num[EVENT_MAX];

/*
 * Set an event callback
 */
static PyObject *event_add_handler(PyObject *self, PyObject *args)
{
        int type, i;
	PyObject *func;

	/* Parse arguments */
	if (!PyArg_ParseTuple(args, "iO", &type, &func))
		return NULL;

	/* Add reference to the new one */
	Py_XINCREF(func);

        for(i = 0; i < python_callbacks_num[type]; i++)
        {
                if(python_callbacks[type][i] == NULL)
                {
                        /* Remember callback */
                        python_callbacks[type][i] = func;

                        /* Return nothing */
                        Py_INCREF(Py_None);
                        return Py_None;
                }
        }

	/* Remember callback */
        python_callbacks[type][python_callbacks_num[type]++] = func;

	/* Return nothing */
	Py_INCREF(Py_None);
	return Py_None;
}

/*
 * Set an event callback
 */
static PyObject *event_remove_handler(PyObject *self, PyObject *args)
{
        int type, i;
	PyObject *func;

	/* Parse arguments */
        if (!PyArg_ParseTuple(args, "iO", &type, &func))
		return NULL;

        for(i = 0; i < python_callbacks_num[type]; i++)
        {
                if(python_callbacks[type][i] == func)
                {
                        /* Delete old callback, if any */
                        Py_XDECREF(python_callbacks[type][i]);

                        /* Remove callback */
                        python_callbacks[type][i] = NULL;
                }
        }

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
        { "remove_handler", event_remove_handler, 1 },
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
	add_constant(d, "EVENT_COMMAND", EVENT_COMMAND);
        add_constant(d, "EVENT_MOVE", EVENT_MOVE);
        add_constant(d, "EVENT_SEARCH", EVENT_SEARCH);
        add_constant(d, "EVENT_ENTER_QUEST", EVENT_ENTER_QUEST);
        add_constant(d, "EVENT_FEELING", EVENT_FEELING);
        add_constant(d, "EVENT_GENERATE_LVL", EVENT_GENERATE_LVL);
        add_constant(d, "EVENT_GO_UP", EVENT_GO_UP);
        add_constant(d, "EVENT_GO_DOWN", EVENT_GO_DOWN);
        add_constant(d, "EVENT_START_GAME", EVENT_START_GAME);
        add_constant(d, "EVENT_AIM_WAND", EVENT_AIM_WAND);
        add_constant(d, "EVENT_USE_STAFF", EVENT_USE_STAFF);
        add_constant(d, "EVENT_ZAP_ROD", EVENT_ZAP_ROD);
        add_constant(d, "EVENT_QUAFF_POTION", EVENT_QUAFF_POTION);
        add_constant(d, "EVENT_READ_SCROLL", EVENT_READ_SCROLL);
        add_constant(d, "EVENT_ACTIVATE_ART", EVENT_ACTIVATE_ART);
        add_constant(d, "EVENT_EAT_FOOD", EVENT_EAT_FOOD);
        add_constant(d, "EVENT_XTRA_POWER", EVENT_XTRA_POWER);
        add_constant(d, "EVENT_MONSTER_DEATH", EVENT_MONSTER_DEATH);
        add_constant(d, "EVENT_ATTACK", EVENT_ATTACK);
	add_constant(d, "EVENT_MAX", EVENT_MAX);
}

/*
 * Call a Python event handler, if it exists
 */
int perform_event(int type, PyObject *args)
{
        int retval = 0, i;
	PyObject *result;

	/* Check for existance of callback */
        for (i = 0; i < python_callbacks_num[type]; i++)
        {
        if (python_callbacks[type][i])
	{
		/* Call function */
                result = PyEval_CallObject(python_callbacks[type][i], args);

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
                                retval += PyInt_AsLong(result);
			}

			/* Delete return object */
			Py_DECREF(result);
		}
	}
        }

	/* Delete arguments */
	Py_XDECREF(args);

	/* Return */
	return (retval);
}
#endif
